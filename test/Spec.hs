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

import Core.RegFile
import Core.Decode
import Core.Execute
import Core.Definitions
import Data.Bool

import Clash.Prelude

registers = Registers { general = repeat (-1) :: Vec 32 XTYPE, pc = 0 :: XTYPE}

topEntity = execute decE
    where
        x = encodeInstruction $ Itype ADD (Register 1) (Register 2) 10
        decD = decodeInstruction x
        decE = decodeInstructionE registers decD

encodeInstructionResult = [0b00000000000111000111000100110111 :: BitVector 32] :> Nil   -- ! TODO: Fix this so that all instruction read result from here

instrArray =  (Utype LUI (Register 2) 0b000111000111,                         0b00000000000111000111000100110111 :: BitVector 32) :> 
              (Utype AUIPC (Register 31) 0b000111000111,                      0b00000000000111000111111110010111) :>
              (Jtype JAL (Register 15) 0b11110000111100001111,                0b11100001111111100001011111101111) :>
              (Itype JALR (Register 1) (Register 2) 26,                       0b00000001101000001000000101100111) :>
              (Branch BEQ (Register 10) (Register 20) 0b101010101010,         0b11010101010001010000101001100011) :>
              (Branch BGEU (Register 10) (Register 20) 0b101010101010,        0b11010101010001010111101001100011) :>
              (Itype SLT (Register 5) (Register 15) 0b111100001111,           0b11110000111100101010011110010011) :> 
              (Itype SL (Register 5) (Register 15) 0b000000010001,            0b00000001000100101001011110010011) :>
              (Itype SRL (Register 5) (Register 15) 0b000000010001,           0b00000001000100101101011110010011) :>
              (Itype SRA (Register 5) (Register 15) 0b010000010001,           0b01000001000100101101011110010011) :>
              (Rtype XOR (Register 5) (Register 1) (Register 15) 0,           0b00000000000100101100011110110011) :>
              (Rtype SUB (Register 5) (Register 1) (Register 15) 0b0100000,   0b01000000000100101000011110110011) :>
              (Rtype SRA (Register 5) (Register 1) (Register 15) 0b0100000,   0b01000000000100101101011110110011) :>
              (Rtype SRL (Register 5) (Register 1) (Register 15) 0,           0b00000000000100101101011110110011) :>
              Nil

checkEncode i = encodeInstruction (fst (instrArray !! i)) `shouldBe` (snd (instrArray !! i))
checkDecodeD i = decodeInstruction (snd (instrArray !! i)) `shouldBe` (fst (instrArray !! i))

main :: IO ()
main = hspec $ do
  describe "Core.Decode.encode" $ do
    it "encodes LUI"    $ do   checkEncode 0
    it "encodes AUIPC"  $ do   checkEncode 1
    it "encodes JAL"    $ do   checkEncode 2
    it "encodes JALR"   $ do   checkEncode 3
    it "encodes BEQ"    $ do   checkEncode 4
    it "encodes BGEU"   $ do   checkEncode 5
    it "encodes SLTI"   $ do   checkEncode 6
    it "encodes SLLI"   $ do   checkEncode 7
    it "encodes SRLI"   $ do   checkEncode 8
    it "encodes SRAI"   $ do   checkEncode 9
    it "encodes XOR"    $ do   checkEncode 10
    it "encodes SUB"    $ do   checkEncode 11
    it "encodes SRA"    $ do   checkEncode 12
    it "encodes SRL"    $ do   checkEncode 13

  describe "Core.Decode.decodeD" $ do
    it "decodes LUI"    $ do   checkDecodeD 0
    it "decodes AUIPC"  $ do   checkDecodeD 1
    it "decodes JAL"    $ do   checkDecodeD 2
    it "decodes JALR"   $ do   checkDecodeD 3
    it "decodes BEQ"    $ do   checkDecodeD 4
    it "decodes BGEU"   $ do   checkDecodeD 5
    it "decodes SLTI"   $ do   checkDecodeD 6
    it "decodes SLLI"   $ do   checkDecodeD 7
    it "decodes SRLI"   $ do   checkDecodeD 8
    it "decodes SRAI"   $ do   checkDecodeD 9
    it "decodes XOR"    $ do   checkDecodeD 10
    it "decodes SUB"    $ do   checkDecodeD 11
    it "decodes SRA"    $ do   checkDecodeD 12
    it "decodes SRL"    $ do   checkDecodeD 13

  describe "Core.Decode.decodeE" $ do
    it "needs testing" $ do
      pending