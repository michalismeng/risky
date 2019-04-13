{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Core.Definitions where

import Prelude (Show, Eq)
import Clash.Sized.Unsigned (Unsigned)
import Clash.Sized.Signed (Signed)
import Clash.Sized.BitVector (BitVector)
import Clash.Sized.Index
import Clash.Sized.Vector (Vec((:>), Nil), repeat, (++))
import GHC.TypeLits.Extra(CLog)

import Control.DeepSeq (NFData)

type XLEN = 32
type XREGLEN = CLog 2 XLEN
type XTYPE = BitVector XLEN
type XREGPTR = BitVector XREGLEN
type XOpcode2 = BitVector 3
type XUnsigned = Unsigned XLEN
type XSigned = Signed XLEN

data Signedness = Signed 
                | Unsigned
                deriving Show

data Register = Register (Index XLEN)
              | PC                            
              deriving (Show, Eq)

data Registers = Registers {
                             general :: Vec 32 XTYPE, 
                             pc :: XTYPE }       
                             deriving Show

type ICache = Vec 64 XTYPE

data Opcode
    = LUI       -- uType
    | AUIPC
    | JAL       -- Jtype
    | JALR
    | ADD       -- irType
    | SUB
    | SLT
    | SLTU
    | XOR
    | OR
    | AND
    | SL
    | SRA
    | SRL
    | BEQ       -- bType
    | BNE
    | BLT
    | BLTU
    | BGE
    | BGEU      
    | LB        -- memory type
    | LH
    | LW
    | LBU
    | LHU
    | SB
    | SH
    | SW
    | NOP       -- dummy instruction used to pad data structures - not in specification
    deriving (Show, Eq)
      
data Instruction register
    = Branch    Opcode register register (BitVector 12)
    | Itype     Opcode register Register (BitVector 12)
    | Rtype     Opcode register register Register (BitVector 7)
    | Utype     Opcode register (BitVector 20)
    | Jtype     Opcode Register (BitVector 20)
    | Load      Opcode register Register (BitVector 12)
    | Store     Opcode register register (BitVector 12)
    deriving (Show, Eq)

data InstructionE
    = BranchE       Opcode XSigned XSigned XSigned XSigned  -- Operand a, Operand b, PC, PC Offset
    | ArithmeticE   Opcode XSigned XSigned Register         -- Operand a, Operand b, Destination Register
    | UtypeE        Opcode XSigned XSigned Register         -- Upper Immediate, PC
    | JumpE         Opcode XSigned XSigned Register         -- PC Offset, PC
    | LoadE         Opcode XSigned XSigned Register         -- Memory Base, Memory Offset, Destination Register
    | StoreE        Opcode XSigned XSigned XSigned          -- Memory Base, Memory Offset, Source Register
    deriving (Show, Eq)

type InstructionD = Instruction Register

data MemoryResult
    = ReadMemory   Opcode XUnsigned Register                   -- Memory address, Destination Register
    | WriteMemory  Opcode XUnsigned XSigned                    -- Memory address, Write Value
    | NoMemOp      Result                                      -- (Used by instructions that do not access memory)
    deriving Show

data Result
    = ChangeReg  XSigned Register                            -- Change the value of the given register to the given XSigned value
    | ChangeReg2 XSigned Register XSigned Register           -- Change the value of the two given registers
    deriving Show

data CPUActivity
    = Fetch
    | Decode XTYPE
    | Execute InstructionE
    | Memory MemoryResult 
    | WriteBack Result
    deriving Show

data CPUState = CPUState CPUActivity Registers deriving Show

op_lui      = 0b0110111 :: BitVector 7
op_auipc    = 0b0010111 :: BitVector 7
op_jal      = 0b1101111 :: BitVector 7
op_jalR     = 0b1100111 :: BitVector 7
op_branch   = 0b1100011 :: BitVector 7
op_load     = 0b0000011 :: BitVector 7
op_store    = 0b0100011 :: BitVector 7
op_itype    = 0b0010011 :: BitVector 7
op_rtype    = 0b0110011 :: BitVector 7
op_fence    = 0b0001111 :: BitVector 7
op_system   = 0b1110011 :: BitVector 7

-- only instructions with unique funct3 opcode appear in these lists
-- TODO: All vectors are padded with dummy entries to match the size of 8. Fix this to be variable size (take care of getOpcode function in Decode.hs)

irAssoc :: Vec 8 (XOpcode2, Opcode)
irAssoc = (0b000, ADD)  :> 
          (0b010, SLT)  :>
          (0b011, SLTU) :>  
          (0b100, XOR)  :>  
          (0b110, OR)   :> 
          (0b111, AND)  :> 
          (0b001, SL)   :> 
          (0b101, SRA)  :> Nil
          ++ repeat (0b111, NOP)

bAssoc :: Vec 8 (XOpcode2, Opcode)
bAssoc =  (0b000, BEQ)  :>
          (0b001, BNE)  :>
          (0b100, BLT)  :>
          (0b101, BLTU) :>
          (0b110, BGE)  :>
          (0b111, BGEU) :> Nil               
          ++ repeat (0b111, NOP)     

lAssoc :: Vec 8 (XOpcode2, Opcode)
lAssoc = (0b000, LB)  :>
         (0b001, LH)  :>
         (0b010, LW)  :>
         (0b100, LBU) :>
         (0b101, LHU) :> Nil
         ++ repeat (0b111, NOP)

sAssoc :: Vec 8 (XOpcode2, Opcode)
sAssoc = (0b000, SB)  :>
         (0b001, SH)  :>
         (0b010, SW)  :> Nil
         ++ repeat (0b111, NOP)