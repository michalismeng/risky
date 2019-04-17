module Core.Execute where

import Core.Definitions
import Clash.Prelude

import Debug.Trace

import Data.Bool

alu :: Opcode -> XSigned -> XSigned -> XSigned
alu op a b = case op of
    ADD   -> a + b
    SUB   -> a - b
    SLT   -> bool 0 1 (a < b)
    SLTU  -> bool 0 1 (u a < u b)
    XOR   -> a `xor` b
    OR    -> a .|. b
    AND   -> a .&. b
    SL    -> shiftL a amt
    SRA   -> shiftR a amt
    SRL   -> unpack $ pack $ shiftR (u a) amt :: XSigned
    where
        u x = unpack $ pack x :: XUnsigned
        amt = unpack $ resize $ slice d4 d0 b

bru :: Opcode -> XSigned -> XSigned -> Bool
bru op a b = case op of
    BEQ   -> a == b
    BNE   -> complement $ a == b
    BLT   -> a < b
    BLTU  -> u a < u b
    BGE   -> a > b
    BGEU  -> u a > u b
    where
        u x = unpack $ pack x :: XUnsigned

execute :: InstructionE -> MemoryResult
execute instr = case instr of
    ArithmeticE op a b dst      -> NoMemOp $ ChangeReg res dst                            where res = alu op a b
    BranchE     op a b pc off   -> NoMemOp $ ChangeReg (bool pc (pc + off - 4) res) PC    where res = bru op a b
    UtypeE      op a b dst      -> NoMemOp $ ChangeReg res dst                            where res = bool a (a + b) (op == AUIPC)
    JumpE       op trg pc dst   -> case op of
                                    JALR  -> NoMemOp $ ChangeReg2 pc dst trg PC
                                    JAL   -> NoMemOp $ ChangeReg2 pc dst res PC           where res = trg + pc - 4
    LoadE       op base off dst -> ReadMemory op res dst                                  where res = unpack (pack $ base + off)
    StoreE      op base val off -> WriteMemory op res val                                 where res = unpack (pack $ base + off)