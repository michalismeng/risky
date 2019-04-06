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

registers = Registers { general = 1 :> 0 :> 10 :> repeat (-1) :: Vec 32 XTYPE, pc = 8 :: XTYPE}

instrArray =  (0b00000000000111000111000100110111,  Utype LUI (Register 2) 0b000111000111,                       UtypeE LUI 1863680 8 (Register 2))          :> 
              (0b00000000000111000111111110010111,  Utype AUIPC (Register 31) 0b000111000111,                    UtypeE AUIPC 1863680 8 (Register 31))       :>
              (0b11100001111111100001011111101111,  Jtype JAL (Register 15) 0b11110000111100001111,              JumpE JAL (-123362) 8 (Register 15))        :>
              (0b00000001101000001000000101100111,  Itype JALR (Register 1) (Register 2) 26,                     JumpE JALR 26 8 (Register 2))               :>
              (0b11010101010001010000101001100011,  Branch BEQ (Register 10) (Register 20) 0b101010101010,       BranchE BEQ (-1) (-1) 8 (-2732))            :>
              (0b11010101010001010111101001100011,  Branch BGEU (Register 10) (Register 20) 0b101010101010,      BranchE BGEU (-1) (-1) 8 (-2732))           :>
              (0b11110000111100101010011110010011,  Itype SLT (Register 5) (Register 15) 0b111100001111,         ArithmeticE SLT (-1) (-241) (Register 15))  :> 
              (0b00000001000100101001011110010011,  Itype SL (Register 5) (Register 15) 0b000000010001,          ArithmeticE SL (-1) 17 (Register 15))       :>
              (0b00000001000100101101011110010011,  Itype SRL (Register 5) (Register 15) 0b000000010001,         ArithmeticE SRL (-1) 17 (Register 15))      :>
              (0b01000001000100101101011110010011,  Itype SRA (Register 5) (Register 15) 0b010000010001,         ArithmeticE SRA (-1) 1041 (Register 15))    :>
              (0b00000000000100101100011110110011,  Rtype XOR (Register 5) (Register 1) (Register 15) 0,         ArithmeticE XOR (-1) 0 (Register 15))       :>
              (0b01000000000100101000011110110011,  Rtype SUB (Register 5) (Register 1) (Register 15) 0b0100000, ArithmeticE SUB (-1) 0 (Register 15))       :>
              (0b01000000000100101101011110110011,  Rtype SRA (Register 5) (Register 1) (Register 15) 0b0100000, ArithmeticE SRA (-1) 0 (Register 15))       :>
              (0b00000000000100101101011110110011,  Rtype SRL (Register 5) (Register 1) (Register 15) 0,         ArithmeticE SRL (-1) 0 (Register 15))       :>
              Nil

sfst (x,_,_) = x
ssnd (_,x,_) = x
strd (_,_,x) = x

decE = decodeInstructionE registers

checkEncode i = encodeInstruction (ssnd el) `shouldBe` (sfst el)    where el = instrArray !! i
checkDecodeD i = decodeInstruction (sfst el) `shouldBe` (ssnd el)   where el = instrArray !! i
checkDecodeE i = decE (ssnd el) `shouldBe` (strd el)                where el = instrArray !! i  

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
    it "decodes LUI"    $ do   checkDecodeE 0
    it "decodes AUIPC"  $ do   checkDecodeE 1
    it "decodes JAL"    $ do   checkDecodeE 2
    it "decodes JALR"   $ do   checkDecodeE 3
    it "decodes BEQ"    $ do   checkDecodeE 4
    it "decodes BGEU"   $ do   checkDecodeE 5
    it "decodes SLTI"   $ do   checkDecodeE 6
    it "decodes SLLI"   $ do   checkDecodeE 7
    it "decodes SRLI"   $ do   checkDecodeE 8
    it "decodes SRAI"   $ do   checkDecodeE 9
    it "decodes XOR"    $ do   checkDecodeE 10
    it "decodes SUB"    $ do   checkDecodeE 11
    it "decodes SRA"    $ do   checkDecodeE 12
    it "decodes SRL"    $ do   checkDecodeE 13