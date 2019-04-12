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
import Core.Execute
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
    Nil

simpleProgramMem :: Vec 16 XTYPE
simpleProgramMem = fmap encodeInstruction simpleProgram ++ repeat 0

defaultCPUState = CPUState Fetch (Registers { general = repeat 0, pc = 0})

simulation = fmap (\(pc, x) -> (unpack pc :: XSigned, fmap unpack x :: [XSigned])) $ sampleN 92 $ cpuHardware defaultCPUState simpleProgramMem

main :: IO ()
main = hspec $ do
  describe "System-Simple CPU" $ do
    it "calculates 5 first natural numbers"    $ do P.last simulation == (24, [0, 0, 6, 15, 0, 0])