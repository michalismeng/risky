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

-- logic simulation
samples = [
  sampleN 150 $ cpuHardware defaultCPUState (pad nat_sum_ICache) (pad nat_sum_DCache),
  sampleN 210 $ cpuHardware defaultCPUState (pad fib_ICache) (pad fib_DCache),
  sampleN 150 $ cpuHardware defaultCPUState (pad prod_ICache) (pad prod_DCache),
  sampleN 150 $ cpuHardware defaultCPUState (pad func_ICache) (pad func_DCache)
  ]

simulations = P.fmap (\x -> fmap mapOutRegisters $ catMaybes $ x) samples

-- memory simulation
-- simulation3 = fmap (\(pc, x) -> (unpack pc :: XSigned, fmap unpack x :: [XSigned])) $ catMaybes $ sampleN 150 $ cpuHardware defaultCPUState (riscv2_ICache ++ repeat 0) ( ( riscv2_DCache) ++ repeat 0)
-- simulation4 = fmap (\(pc, x) -> (unpack pc :: XSigned, fmap unpack x :: [XSigned])) $ catMaybes $ sampleN 150 $ cpuHardware defaultCPUState (riscv3_ICache ++ repeat 0) ( ( riscv3_DCache) ++ repeat 0)

main :: IO ()
main = hspec $ do
  describe "System-Simple CPU" $ do
    it "calculates sum of 5 first natural numbers"    $ do P.last (simulations P.!! 0) `shouldBe` (28, [0, 0, 6, 15, 0, 0])
    it "calculates 5th fibonacci number"              $ do P.last (simulations P.!! 1) `shouldBe` (44, [0, 3, 5, 6, 5, 6])
    it "calculates product of 5, 7"                   $ do P.last (simulations P.!! 2) `shouldBe` (32, [0, 5, 7, 35, 0, 0])
    it "call dummy function"                          $ do P.last (simulations P.!! 3) `shouldBe` (16, [0, 10, 25, 35, 35, 12])
    -- it "loads words from memory" $ do P.last simulation3 `shouldBe` (20, [0, 4, 0x12341234, 0x45674567, 0x12341234, 0x45674567])
    -- it "loads half words and bytes from memory" $ do P.last simulation4 `shouldBe` (20, [0, 4, 0x1234, 0x4567, 0x67, 0x67])       -- * memory is word-addressable (check riscv3.asm - final register should have value 0x45)


-- ******* Beginning of auto generated compiled code *******
func_ICache = 
  0b00000000101000000000000010010011 :>
  0b00000001100100000000000100010011 :>
  0b00000000110000000000001011101111 :>
  0b00000000001000001000001000110011 :>
  0b00000000000000000000000001101111 :>
  0b00000000001000001000000110110011 :>
  0b00000000000000101000000001100111 :>
  Nil
func_DCache = 
  Nil
nat_sum_ICache = 
  0b00000000010100000000000010010011 :>
  0b00000000000100000000000100010011 :>
  0b00000000000000000000000110110011 :>
  0b00000000001100010000000110110011 :>
  0b00000000000100010000000100010011 :>
  0b11111111111100001000000010010011 :>
  0b11111110000000001001101011100011 :>
  0b00000000000000000000000001100011 :>
  Nil
nat_sum_DCache = 
  Nil
fib_ICache = 
  0b00000000000100000000000010010011 :>
  0b00000000000000000000000100110011 :>
  0b00000000000100000000000110010011 :>
  0b00000000011000000000001010010011 :>
  0b00000000010100011010001100110011 :>
  0b00000000000000110000110001100011 :>
  0b00000000000100010000001000110011 :>
  0b00000000000000010000000010110011 :>
  0b00000000000000100000000100110011 :>
  0b00000000000100011000000110010011 :>
  0b11111110100111111111000001101111 :>
  0b00000000000000000000000001101111 :>
  Nil
fib_DCache = 
  Nil
prod_ICache = 
  0b00000000010100000000000010010011 :>
  0b00000000011100000000000100010011 :>
  0b00000000000000000000000110110011 :>
  0b00000000000100000000001000110011 :>
  0b00000000000000100000100001100011 :>
  0b00000000001100010000000110110011 :>
  0b11111111111100100000001000010011 :>
  0b11111111010111111111000001101111 :>
  0b00000000000000000000000001100011 :>
  Nil
prod_DCache = 
  Nil
