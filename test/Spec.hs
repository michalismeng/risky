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

-- calculate sum of N first natural numbers
simpleProgram = 
    Itype   ADD (Register 0) (Register 1) 5               :>              -- R1 = n = 5       
    Itype   ADD (Register 0) (Register 2) 1               :>              -- R2 = i: current natural
    Rtype   ADD (Register 0) (Register 0) (Register 3) 0  :>              -- R3 = s: current sum

    Rtype   ADD (Register 3) (Register 2) (Register 3) 0  :>              -- jump target
    Itype   ADD (Register 2) (Register 2) 1               :>
    Itype   ADD (Register 1) (Register 1) (-1)            :>
    Branch  BNE (Register 1) (Register 0) (-8)            :>              -- -8 * 2 (required by specification to enforce multiple of 2) = 16 = 4 insts * 4 (pc is incremented)
    Branch  BEQ (Register 0) (Register 0) (-2)            :>
    Nil

simpleProgramMem :: Vec 32 XTYPE
simpleProgramMem = fmap encodeInstruction simpleProgram ++ repeat 0

defaultDCache :: Vec 16 XTYPE
defaultDCache = repeat 0

defaultCPUState = CPUState Fetch (Registers { general = repeat 0, pc = 0})

cpuHardware initialCPU (initialProg :: Vec 32 XTYPE) (initialData :: Vec 16 XTYPE) = output
    where
        state = register (initialCPU, initialProg, initialData) state'
        state' = fmap Core.Pipeline.cycle state

        output = fmap getOutput state'
        getOutput (CPUState s regs, _, _) = case s of
            Fetch -> Just    ((readRegister regs PC - 4), fmap (\i -> readRegister regs (Register i)) [0..5] )
            _     -> Nothing

simulation1 = fmap (\(pc, x) -> (unpack pc :: XSigned, fmap unpack x :: [XSigned])) $ catMaybes $ sampleN 150 $ cpuHardware defaultCPUState simpleProgramMem defaultDCache
simulation2 = fmap (\(pc, x) -> (unpack pc :: XSigned, fmap unpack x :: [XSigned])) $ catMaybes $ sampleN 150 $ cpuHardware defaultCPUState (riscv1_ICache ++ repeat 0) (riscv1_DCache ++ repeat 0)

main :: IO ()
main = hspec $ do
  describe "System-Simple CPU" $ do
    it "calculates sum of 5 first natural numbers"    $ do P.last simulation1 `shouldBe` (24, [0, 0, 6, 15, 0, 0])


-- ******* Beginning of auto generated compiled code *******
riscv1_ICache = 
  0b11111111000000010000000100010011 :>
  0b00000000100100010010001000100011 :>
  0b00000000100000010010010000100011 :>
  0b00000000000100010010011000100011 :>
  0b00000110001100000000010000010011 :>
  0b00000000100000000000010110110011 :>
  0b00000000100000000000011000110011 :>
  0b11111111111101000000010000010011 :>
  0b00000000100000000000011010110011 :>
  0b00001111110000010000010100010111 :>
  0b11111101110001010000010100010011 :>
  0b11111110000001000001010011100011 :>
  0b00000000110000010010000010000011 :>
  0b00000000100000010010010000000011 :>
  0b00000000010000010010010010000011 :>
  0b00000001000000010000000100010011 :>
  0b00000000000000000000010100010011 :>
  0b00000101110100000000100010010011 :>
  Nil
riscv1_DCache = 
  0b01101100011011000110010101101000 :>
  0b00000000000000000000101001101111 :>
  0b00000000000000000000000000000000 :>
  0b00000000000000000000000000000000 :>
  0b00000000000000000000000000000000 :>
  0b00000000000000000000000000000000 :>
  0b00000000000000000000000000000000 :>
  0b00000000000000000000000000000000 :>
  0b00000000000000000000000000000000 :>
  0b00000000000000000000000000000000 :>
  0b00000000000000000000000000000000 :>
  0b00000000000000000000000000000000 :>
  0b00000000000000000000000000000000 :>
  0b00000000000000000000000000000000 :>
  0b00000000000000000000000000000000 :>
  0b00000000000000000000000000000000 :>
  Nil
