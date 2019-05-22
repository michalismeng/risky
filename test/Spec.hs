{-# LANGUAGE ScopedTypeVariables #-} 
{-# LANGUAGE NoImplicitPrelude #-} 
{-# LANGUAGE TypeOperators #-} 
{-# LANGUAGE DataKinds #-} 
{-# LANGUAGE FlexibleContexts #-} 
{-# LANGUAGE TypeApplications #-} 
{-# LANGUAGE BinaryLiterals #-} 
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Spec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import qualified Prelude as P

import Core.RegFile
import Core.Decode
import Core.Definitions
import Core.Pipeline
import Data.Bool
import Data.Maybe (catMaybes)

import Clash.Prelude
import Debug.Trace

import Out.Fib
import Out.Func
import Out.Ld_hb
import Out.Ld_word
import Out.Nat_sum
import Out.Prod
import Out.Simple_br
import Out.Simple_jal
import Out.Simple

-- setup cpu hardware used for testing. Output PC and registers x0-x5 at each cycle
cpuHardware initialCPU (initialProg :: Vec 32 XTYPE) (initialData :: Vec 16 XTYPE) = output
    where
        state = register (initialCPU, initialProg, initialData) state'
        state' = fmap Core.Pipeline.cycle state

        output = fmap getOutput state
        getOutput (CPUState s regs, _, _) = case s of
            Fetch -> Just ((readRegister regs PC), fmap (\i -> readRegister regs (Register i)) [0..5] )
            _     -> Nothing
           
defaultCPUState = CPUState Fetch (Registers { general = repeat 0, pc = 0})

mapOutRegisters (pc, x) = (unpack pc :: XSigned, fmap unpack x :: [XSigned])
pad x = x ++ repeat 0

samples = [
  sampleN 150 $ cpuHardware defaultCPUState (pad nat_sum_ICache) (pad nat_sum_DCache),
  sampleN 210 $ cpuHardware defaultCPUState (pad fib_ICache) (pad fib_DCache),
  sampleN 150 $ cpuHardware defaultCPUState (pad prod_ICache) (pad prod_DCache),
  sampleN 150 $ cpuHardware defaultCPUState (pad func_ICache) (pad func_DCache),
  sampleN 150 $ cpuHardware defaultCPUState (pad ld_word_ICache) (pad ld_word_DCache),
  sampleN 150 $ cpuHardware defaultCPUState (pad ld_hb_ICache) (pad ld_hb_DCache)
  ]

simulations = P.fmap (\x -> fmap mapOutRegisters $ catMaybes $ x) samples

main :: IO ()
main = hspec $ do
  describe "System-Simple CPU" $ do
    it "calculates sum of 5 first natural numbers"    $ do P.last (simulations P.!! 0) `shouldBe` (28, [0, 0, 6, 15, 0, 0])
    it "calculates 5th fibonacci number"              $ do P.last (simulations P.!! 1) `shouldBe` (44, [0, 3, 5, 6, 5, 6])
    it "calculates product of 5, 7"                   $ do P.last (simulations P.!! 2) `shouldBe` (32, [0, 5, 7, 35, 0, 0])
    it "call dummy function"                          $ do P.last (simulations P.!! 3) `shouldBe` (16, [0, 10, 25, 35, 35, 12])
    it "loads words from memory"                      $ do P.last (simulations P.!! 4) `shouldBe` (24, [0, 4, 0x12341234, 0x45674567, 0x12341234, 0x45674567])
    it "loads half words and bytes from memory"       $ do P.last (simulations P.!! 5) `shouldBe` (24, [0, 4, 0x1234, 0x4567, 0x67, 0x67])       -- * memory is word-addressable (check ld_hb.asm - final register should have value 0x45)