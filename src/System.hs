{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE GADTs #-}

module System where

import Core.ALU
import Core.RegFile
import Core.Decode
import Core.Definitions
import Data.Bool

import Prelude ()
import Clash.Prelude

registers = Registers { general = repeat (-1) :: Vec 32 XTYPE, pc = 0 :: XTYPE}

topEntity = execute decE
    where
        x = encodeInstruction $ Itype SRL (Register 1) (Register 2) 0b000000000001
        decD = decodeInstruction x
        decE = decodeInstructionE registers decD