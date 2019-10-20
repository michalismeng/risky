{-# LANGUAGE NoImplicitPrelude, DataKinds, BinaryLiterals #-}

module Out.St_word where

import Clash.Prelude

st_word_ICache = 
  (0b00001111110000010000000010010111 :: BitVector 32) :>
  (0b00000000000000001000000010010011 :: BitVector 32) :>
  (0b11011110101011011100000100110111 :: BitVector 32) :>
  (0b10101011101000010000000100010011 :: BitVector 32) :>
  (0b10001011101011011111000110110111 :: BitVector 32) :>
  (0b00000000110100011000000110010011 :: BitVector 32) :>
  (0b00000000001000001010000000100011 :: BitVector 32) :>
  (0b00000000001100001010001000100011 :: BitVector 32) :>
  (0b00000000100000001000000010010011 :: BitVector 32) :>
  (0b00000000001100001010000000100011 :: BitVector 32) :>
  (0b11111111100000001000000010010011 :: BitVector 32) :>
  (0b00000000000000001010001000000011 :: BitVector 32) :>
  (0b00000000010000001010001010000011 :: BitVector 32) :>
  (0b00000000100000001010001100000011 :: BitVector 32) :>
  (0b00000000000000000000000001100011 :: BitVector 32) :>
  Nil
st_word_DCache = 
  (0b00010010001101000101011001111000 :: BitVector 32) :>
  (0b01000101011001111000100110101011 :: BitVector 32) :>
  (0b00000000000000000000000000000000 :: BitVector 32) :>
  (0b00000000000000000000000000000000 :: BitVector 32) :>
  (0b00000000000000000000000000000000 :: BitVector 32) :>
  (0b00000000000000000000000000000000 :: BitVector 32) :>
  (0b00000000000000000000000000000000 :: BitVector 32) :>
  (0b00000000000000000000000000000000 :: BitVector 32) :>
  (0b00000000000000000000000000000000 :: BitVector 32) :>
  (0b00000000000000000000000000000000 :: BitVector 32) :>
  (0b00000000000000000000000000000000 :: BitVector 32) :>
  (0b00000000000000000000000000000000 :: BitVector 32) :>
  (0b00000000000000000000000000000000 :: BitVector 32) :>
  (0b00000000000000000000000000000000 :: BitVector 32) :>
  (0b00000000000000000000000000000000 :: BitVector 32) :>
  (0b00000000000000000000000000000000 :: BitVector 32) :>
  Nil