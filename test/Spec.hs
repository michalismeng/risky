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

import Prelude ()

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

main :: IO ()
main = hspec $ do
  describe "System" $ do
    it "does addition" $ do
      topEntity `shouldBe` (9 :: XSigned)
