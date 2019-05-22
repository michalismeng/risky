{-# LANGUAGE NoImplicitPrelude, DataKinds, BinaryLiterals #-}

module Out.Simple_lui where

import Clash.Prelude

simple_lui_ICache = 
  (0b01000000100000010001000010110111 :: BitVector 32) :>
  (0b00000000000000001111000100010111 :: BitVector 32) :>
  (0b00000000000100010000000110010011 :: BitVector 32) :>
  (0b00000000000000000000000000110011 :: BitVector 32) :>
  (0b00000000000000000000000000110011 :: BitVector 32) :>
  (0b00000000000000000000000000110011 :: BitVector 32) :>
  (0b00000000000000000000000001100011 :: BitVector 32) :>
  Nil
simple_lui_DCache = 
  Nil
