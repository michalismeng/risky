module Core.Decode where

import Core.Definitions
import Core.RegFile
import Core.ALU

import Prelude ()
import Data.Bool
import Data.Maybe

import Clash.Prelude

opcode :: XTYPE -> BitVector 7
opcode = slice d6 d0

rd :: XTYPE -> BitVector XREGLEN
rd = slice d11 d7

funct3 :: XTYPE -> BitVector 3
funct3 = slice d14 d12

rs1 :: BitVector 32 -> BitVector 5
rs1 = slice d19 d15

rs2 :: XTYPE -> BitVector XREGLEN
rs2 = slice d24 d20

funct7 :: XTYPE -> BitVector 7
funct7 = slice d31 d25

iImm :: XTYPE -> BitVector 12
iImm = slice d31 d20

sImm :: XTYPE -> BitVector 12
sImm x = slice d31 d25 x ++# slice d11 d7 x

bImm :: XTYPE -> BitVector 12
bImm x = slice d31 d31 x ++# slice d7 d7 x ++# slice d30 d25 x ++# slice d11 d8 x

uImm :: XTYPE -> BitVector 20
uImm = slice d31 d12

jImm :: XTYPE -> BitVector 20
jImm x = slice d31 d31 x ++# slice d19 d12 x ++# slice d20 d20 x ++# slice d30 d21 x

lui     =  (== op_lui   ) . opcode
auipc   =  (== op_auipc ) . opcode
jal     =  (== op_jal   ) . opcode
branch  =  (== op_branch) . opcode
load    =  (== op_load  ) . opcode
store   =  (== op_store ) . opcode
iType   =  (== op_itype ) . opcode
rType   =  (== op_rtype ) . opcode
fence   =  (== op_fence ) . opcode
system  =  (== op_system) . opcode

decodeOpcode :: XTYPE -> Opcode
decodeOpcode instr
    | iType instr || rType instr =  case irOpcode of
        ADD -> bool SUB ADD f7Zero
        SRA -> bool SRA SRL f7Zero 
        _   -> irOpcode
    | branch instr = bOpcode
    where
        f7Zero = funct7 instr == 0
        irOpcode = getOpcode Core.Definitions.irAssoc
        bOpcode  = getOpcode Core.Definitions.bAssoc

        getOpcode assoc = ops !! fromJust ind
            where
                (ops, xops) = (map snd assoc, map fst assoc)
                ind = (funct3 instr) `elemIndex` xops

encodeOpcode :: InstructionD -> XOpcode2
encodeOpcode instr = case instr of
    Itype op _ _ _    -> irXOpcode op
    Rtype op _ _ _ _  -> irXOpcode op
    Branch op _ _ _   -> bXOpcode op
    where
        irXOpcode op = case op of 
            SUB -> getXOpcode Core.Definitions.irAssoc ADD
            SRL -> getXOpcode Core.Definitions.irAssoc SRA
            _   -> getXOpcode Core.Definitions.irAssoc op

        bXOpcode op  = getXOpcode Core.Definitions.bAssoc op

        getXOpcode assoc op = xops !! fromJust ind
            where
                (ops, xops) = (map snd assoc, map fst assoc)
                ind = op `elemIndex` ops

{-# LANGUAGE NOINLINE #-}
decodeInstruction :: XTYPE -> InstructionD
decodeInstruction instruction
    | branch instruction = Branch op2 s1 s2 (bImm instruction)      -- TODO: Add all instruction types
    | load instruction = Load op2 s1 dst (iImm instruction)
    | store instruction = Store op2 s1 s2 (sImm instruction)
    | rType instruction = Rtype op2 s1 s2 dst (funct7 instruction)
    | iType instruction = Itype op2 s1 dst (iImm instruction)
    | otherwise = Clash.Prelude.undefined -- TODO: Exception Bad Instruction
    where
        s1  = decodeRegister $ rs1 instruction
        s2  = decodeRegister $ rs2 instruction
        dst = decodeRegister $ rd instruction
        op2 = decodeOpcode instruction
        decodeRegister = Register . unpack

encodeInstruction :: InstructionD -> XTYPE
encodeInstruction instr = case instr of
    Branch op2 r1 r2 imm   -> slice d11 d11 imm ++# slice d9 d4 imm ++# encodeReg r2 ++# encodeReg r1 ++# xOp2 ++# slice d3 d0 imm ++# slice d10 d10 imm ++# op_branch
    Load op2 r1 rd imm     -> imm ++# encodeReg r1 ++# xOp2 ++# encodeReg rd ++# op_load
    Store op2 r1 r2 imm    -> slice d11 d5 imm ++# encodeReg r2 ++# encodeReg r1 ++# xOp2 ++# slice d4 d0 imm ++# op_store
    Rtype op2 r1 r2 rd f7  -> f7 ++# encodeReg r2 ++# encodeReg r1 ++# xOp2 ++# encodeReg rd ++# op_rtype
    Itype op2 r1 rd imm    -> imm ++# encodeReg r1 ++# xOp2 ++# encodeReg rd ++# op_itype
    where
        encodeReg (Register i) = pack i
        xOp2 = encodeOpcode instr

decodeInstructionE :: Registers -> InstructionD -> InstructionE
decodeInstructionE registers instruction = case instruction of
    Itype  op rs1 rd imm     -> ArithmeticE op (readReg registers rs1) (unpack $ signExtend imm)     rd
    Rtype  op rs1 rs2 rd f7  -> ArithmeticE op (readReg registers rs1) (readReg registers rs2)       rd
    Branch op rs1 rs2 imm    -> BranchE     op (readReg registers rs1) (readReg registers rs2)   (unpack $ signExtend z) where z = (imm ++# (0 :: BitVector 1))
    where
        readReg registers x = unpack (readRegister registers x) :: XSigned