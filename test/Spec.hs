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

-- calculate sum of N first natural numbers
simpleProgram = 
    Itype   ADD (Register 0) (Register 1) 5               :>              -- R1 = n = 5       
    Itype   ADD (Register 0) (Register 2) 1               :>              -- R2 = i: current natural
    Rtype   ADD (Register 0) (Register 0) (Register 3) 0  :>              -- R3 = s: current sum

    Rtype   ADD (Register 3) (Register 2) (Register 3) 0  :>              -- jump target
    Itype   ADD (Register 2) (Register 2) 1               :>
    Itype   ADD (Register 1) (Register 1) (-1)            :>
    Branch  BNE (Register 1) (Register 0) (-6)            :>              -- -8 * 2 (required by specification to enforce multiple of 2) = 16 = 4 insts * 4 (pc is incremented)
    Branch  BEQ (Register 0) (Register 0) 0               :>
    Nil

simpleProgramMem :: Vec 32 XTYPE
simpleProgramMem = fmap encodeInstruction simpleProgram ++ repeat 0

defaultDCache :: Vec 16 (BitVector 32)
defaultDCache = repeat 0

defaultCPUState = CPUState Fetch (Registers { general = repeat 0, pc = 0})

cpuHardware initialCPU (initialProg :: Vec 32 XTYPE) (initialData :: Vec 16 XTYPE) = output
    where
        state = register (initialCPU, initialProg, initialData) state'
        state' = fmap Core.Pipeline.cycle state

        output = fmap getOutput state
        getOutput (CPUState s regs, _, _) = case s of
            Fetch -> Just ((readRegister regs PC), fmap (\i -> readRegister regs (Register i)) [0..5] )
            _     -> Nothing

simulation1 = fmap (\(pc, x) -> (unpack pc :: XSigned, fmap unpack x :: [XSigned])) $ catMaybes $ sampleN 150 $ cpuHardware defaultCPUState simpleProgramMem defaultDCache
simulation2 = fmap (\(pc, x) -> (unpack pc :: XSigned, fmap unpack x :: [XSigned])) $ catMaybes $ sampleN 150 $ cpuHardware defaultCPUState (riscv1_ICache ++ repeat 0) ( ( riscv1_DCache) ++ repeat 0)
simulation3 = fmap (\(pc, x) -> (unpack pc :: XSigned, fmap unpack x :: [XSigned])) $ catMaybes $ sampleN 210 $ cpuHardware defaultCPUState (fib_ICache ++ repeat 0) ( ( fib_DCache) ++ repeat 0)

-- simulation3 = fmap (\(pc, x) -> (unpack pc :: XSigned, fmap unpack x :: [XSigned])) $ catMaybes $ sampleN 150 $ cpuHardware defaultCPUState (riscv2_ICache ++ repeat 0) ( ( riscv2_DCache) ++ repeat 0)
-- simulation4 = fmap (\(pc, x) -> (unpack pc :: XSigned, fmap unpack x :: [XSigned])) $ catMaybes $ sampleN 150 $ cpuHardware defaultCPUState (riscv3_ICache ++ repeat 0) ( ( riscv3_DCache) ++ repeat 0)

main :: IO ()
main = hspec $ do
  describe "System-Simple CPU" $ do
    it "calculates sum of 5 first natural numbers"    $ do P.last simulation1 `shouldBe` (28, [0, 0, 6, 15, 0, 0])
    it "calculates sum of 5 first natural numbers using assembly toolchain"    $ do P.last simulation2 `shouldBe` (28, [0, 0, 6, 15, 0, 0])
    it "calculates 5th fibonacci number" $ do P.last simulation3 `shouldBe` (44, [0, 3, 5, 6, 5, 6])
    -- it "loads words from memory" $ do P.last simulation3 `shouldBe` (20, [0, 4, 0x12341234, 0x45674567, 0x12341234, 0x45674567])
    -- it "loads half words and bytes from memory" $ do P.last simulation4 `shouldBe` (20, [0, 4, 0x1234, 0x4567, 0x67, 0x67])       -- * memory is word-addressable (check riscv3.asm - final register should have value 0x45)


-- ******* Beginning of auto generated compiled code *******
riscv1_ICache = 
  0b00000000010100000000000010010011 :>
  0b00000000000100000000000100010011 :>
  0b00000000000000000000000110110011 :>
  0b00000000001100010000000110110011 :>
  0b00000000000100010000000100010011 :>
  0b11111111111100001000000010010011 :>
  0b11111110000000001001101011100011 :>
  0b00000000000000000000000001100011 :>
  Nil
riscv1_DCache = 
  Nil
riscv2_ICache = 
  0b00000000000000000000000010010011 :>
  0b00000000000000001010000100000011 :>
  0b00000000010000001010000110000011 :>
  0b00000000010000001000000010010011 :>
  0b11111111110000001010001000000011 :>
  0b00000000000000001010001010000011 :>
  0b00000000000000000000000001100011 :>
  Nil
riscv2_DCache = 
  0b00010010001101000001001000110100 :>
  0b01000101011001110100010101100111 :>
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
riscv3_ICache = 
  0b00000000000000000000000010010011 :>
  0b00000000000000001001000100000011 :>
  0b00000000010000001001000110000011 :>
  0b00000000010000001000000010010011 :>
  0b00000000000000001000001000000011 :>
  0b00000000000100001000001010000011 :>
  0b00000000000000000000000001100011 :>
  Nil
riscv3_DCache = 
  0b00010010001101000001001000110100 :>
  0b01000101011001110100010101100111 :>
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
