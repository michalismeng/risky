module Core.Execute where

import Core.Definitions
import Clash.Prelude

import Data.Bool

alu :: Opcode -> BitVector 32 -> BitVector 32 -> BitVector 32
alu op a b = case op of
    ADD   -> a + b
    SUB   -> a - b
    SLT   -> bool 0 1 (s a < s b)
    SLTU  -> bool 0 1 (u a < u b)
    XOR   -> a `xor` b
    OR    -> a .|. b
    AND   -> a .&. b
    SL    -> shiftL a amt
    SRA   -> shiftR a amt
    SRL   -> pack $ shiftR (unpack a :: Signed 32) amt
    _     -> a
    where
        u x = unpack $ pack x :: XUnsigned
        s x = unpack $ pack x :: XSigned
        amt = unpack $ zeroExtend $ slice d4 d0 b

bru :: Opcode -> BitVector 32 -> BitVector 32 -> Bool
bru op a b = case op of
    BEQ   -> a == b
    BNE   -> complement $ a == b
    BLT   -> s a < s b
    BLTU  -> u a < u b
    BGE   -> s a > s b
    BGEU  -> u a > u b
    _     -> False
    where
        u x = unpack $ pack x :: XUnsigned
        s x = unpack $ pack x :: XSigned