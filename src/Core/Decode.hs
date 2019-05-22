module Core.Decode where

import Core.Definitions
import Core.RegFile
import Core.Execute

import Data.Bool
import Data.Maybe

import Clash.Prelude

import qualified Prelude as P

import Debug.Trace


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
jalR    =  (== op_jalR  ) . opcode
branch  =  (== op_branch) . opcode
load    =  (== op_load  ) . opcode
store   =  (== op_store ) . opcode
iType   =  (== op_itype ) . opcode
rType   =  (== op_rtype ) . opcode
uType x =  opcode x == op_lui || opcode x == op_auipc
fence   =  (== op_fence ) . opcode
system  =  (== op_system) . opcode

decodeOpcode :: XTYPE -> Opcode
decodeOpcode instr
    | iType instr || rType instr =  case irOpcode of
        ADD -> bool SUB ADD f7Zero
        SRA -> bool SRA SRL f7Zero 
        _   -> irOpcode
    | branch instr = bOpcode
    | load instr  = lOpcode
    | store instr = sOpcode
    | uType instr = bool LUI AUIPC (opcode instr == op_auipc)
    where
        irOpcode = getOpcode Core.Definitions.irAssoc
        bOpcode  = getOpcode Core.Definitions.bAssoc
        lOpcode  = getOpcode Core.Definitions.lAssoc
        sOpcode  = getOpcode Core.Definitions.sAssoc
        f7Zero = funct7 instr == 0

        getOpcode assoc = ops !! fromJust ind
            where
                (ops, xops) = (map snd assoc, map fst assoc)
                ind = (funct3 instr) `elemIndex` xops

encodeOpcode :: InstructionD -> XOpcode2
encodeOpcode instr = case instr of
    Itype op _ _ _    -> bool (irXOpcode op) 0 (op == JALR)
    Rtype op _ _ _ _  -> irXOpcode op
    Branch op _ _ _   -> bXOpcode op
    Load   op _ _ _   -> lXOpcode op
    Store  op _ _ _   -> sXOpcode op
    where
        irXOpcode op = case op of 
            SUB -> getXOpcode Core.Definitions.irAssoc ADD
            SRL -> getXOpcode Core.Definitions.irAssoc SRA
            _   -> getXOpcode Core.Definitions.irAssoc op
        bXOpcode op  = getXOpcode Core.Definitions.bAssoc op
        lXOpcode op  = getXOpcode Core.Definitions.lAssoc op
        sXOpcode op  = getXOpcode Core.Definitions.sAssoc op

        getXOpcode assoc op = xops !! fromJust ind
            where
                (ops, xops) = (map snd assoc, map fst assoc)
                ind = op `elemIndex` ops

decodeInstruction :: XTYPE -> InstructionD
decodeInstruction instruction
    | branch instruction = Branch op2 s1 s2 (bImm instruction)
    | load instruction   = Load  op2 s1 dst (iImm instruction)
    | store instruction  = Store op2 s1 s2 (sImm instruction)
    | rType instruction  = Rtype op2 s1 s2 dst (funct7 instruction)
    | iType instruction  = Itype op2' s1 dst (iImm instruction) 
    | uType instruction  = Utype op2 dst (uImm instruction)
    | jal instruction    = Jtype JAL dst (jImm instruction)
    | jalR instruction   = Itype JALR s1 dst (iImm instruction)
    | otherwise = error ("Unknown instruction type " P.++ (show instruction))                 -- TODO: Exception Bad Instruction
    where
        s1  = decodeRegister $ rs1 instruction
        s2  = decodeRegister $ rs2 instruction
        dst = decodeRegister $ rd instruction
        op2 = decodeOpcode instruction
        op2' = bool op2 ADD (op2 == SUB)
        decodeRegister = Register . unpack

encodeInstruction :: InstructionD -> XTYPE
encodeInstruction instr = case instr of
    Branch op2 r1 r2 imm   -> slice d11 d11 imm ++# slice d9 d4 imm ++# encodeReg r2 ++# encodeReg r1 ++# xOp2 ++# slice d3 d0 imm ++# slice d10 d10 imm ++# op_branch
    Load op2 r1 rd imm     -> imm ++# encodeReg r1 ++# xOp2 ++# encodeReg rd ++# op_load
    Store op2 r1 r2 imm    -> slice d11 d5 imm ++# encodeReg r2 ++# encodeReg r1 ++# xOp2 ++# slice d4 d0 imm ++# op_store
    Rtype op2 r1 r2 rd f7  -> f7 ++# encodeReg r2 ++# encodeReg r1 ++# xOp2 ++# encodeReg rd ++# op_rtype
    Itype op2 r1 rd imm    -> imm ++# encodeReg r1 ++# xOp2 ++# encodeReg rd ++# bool op_itype op_jalR (op2 == JALR)    -- TODO: Fix imm for Shift instructions (user should not specify upper bits but only shamt)
    Jtype op rd imm        -> (slice d19 d19 imm) ++# (slice d9 d0 imm) ++# (slice d10 d10 imm) ++# (slice d18 d11 imm) ++# encodeReg rd ++# op_jal
    Utype op rd imm        -> imm ++# encodeReg rd ++# bool op_lui op_auipc (op == AUIPC)
    where
        encodeReg (Register i) = pack i
        xOp2 = encodeOpcode instr

decodeInstructionE :: Registers -> InstructionD -> InstructionE
decodeInstructionE registers instruction = case instruction of
    Itype  op rs1 rd imm     -> case op of 
                                    JALR -> JumpE       op (unpack (slice d31 d1 z ++# z1)) (readReg PC) rd    where z = pack ((unpack $ signExtend imm) + readReg rs1)
                                    _    -> ArithmeticE op (readReg rs1) (unpack $ signExtend imm) rd
    Load   op rs1 rd imm     -> LoadE       op (readReg rs1)           (unpack $ signExtend imm)   rd
    Store  op rs1 rs2 imm    -> StoreE      op (readReg rs1)           (readReg rs2)   (unpack $ signExtend imm)
    Rtype  op rs1 rs2 rd f7  -> ArithmeticE op (readReg rs1)           (readReg rs2)   rd
    Branch op rs1 rs2 imm    -> BranchE     op (readReg rs1)           (readReg rs2)  (readReg PC)   (unpack $ signExtend z) where z = imm ++# z1
    Utype  op rd imm         -> UtypeE      op (unpack z)              (readReg PC)    rd                                    where z = imm ++# z12
    Jtype  op rd imm         -> JumpE       op (unpack $ signExtend z) (readReg PC)    rd                                    where z = imm ++# z1
    where
        readReg PC = unpack (readRegister registers PC)
        readReg x  = unpack (readRegister registers x)
        z1  = 0 :: BitVector 1
        z12 = 0 :: BitVector 12

decodeAluOpcode instr 
    | rType instr = 
        case funct3 instr of
            0b000 -> bool ADD SUB (unpack $ slice d30 d30 instr)
            0b001 -> SL
            0b010 -> SLT
            0b011 -> SLTU
            0b100 -> XOR
            0b101 -> bool SRL SRA (unpack $ slice d30 d30 instr)
            0b110 -> OR
            0b111 -> AND
    | iType instr = 
        case funct3 instr of
            0b000 -> ADD
            0b001 -> SL
            0b010 -> SLT
            0b011 -> SLTU
            0b100 -> XOR
            0b101 -> bool SRL SRA (unpack $ slice d30 d30 instr)
            0b110 -> OR
            0b111 -> AND
    | jalR instr  = ADD
    | auipc instr = ADD
    | otherwise   = NOP

decodeBruOpcode instr
    | branch instr = 
        case funct3 instr of
            0b000 -> BEQ
            0b001 -> BNE
            0b100 -> BLT
            0b101 -> BGE
            0b110 -> BLTU
            0b111 -> BGEU
            _     -> NOP
    | otherwise = NOP
    
usesAlu, usesRs1, usesRs2, usesRd :: XTYPE -> Bool
usesAlu instr = iType instr || rType instr
usesRs1 instr = jalR instr || branch instr || load instr || store instr || iType instr || rType instr
usesRs2 instr = branch instr || store instr || rType instr
usesRd  instr = lui instr || auipc instr || jal instr || jalR instr || load instr || iType instr || rType instr