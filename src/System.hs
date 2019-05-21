module System where

import Core.RegFile
import Core.Fetch
import Core.Decode
import Core.Execute
import Core.Writeback
import Core.Definitions
import Core.Pipeline
import Data.Bool
import Data.Maybe (catMaybes)

import Debug.Trace

import Clash.Prelude

import qualified Prelude as P

import Control.DeepSeq (NFData)

-- Hack from https://github.com/adamwalker/clash-riscv
-- fixes ROM initial value ? 
firstCycleDef' :: HiddenClockReset dom sync gated => a -> Signal dom a -> Signal dom a
firstCycleDef' defa = mealy step False
    where
    step False _ = (True, defa)
    step True  x = (True, x)

firstCycleDef :: (HiddenClockReset dom sync gated, Default a) => Signal dom a -> Signal dom a
firstCycleDef = firstCycleDef' def
-----

cpuHardware :: HiddenClockReset dom gated sync => Vec (2 ^ 5) (BitVector 32) -> Signal dom (Vec 10 XTYPE)
cpuHardware initialProg = output
    where
        instr_0 = firstCycleDef $ romPow2 initialProg (unpack . resize <$> next_pc_0)
        (regFile, next_pc) = pipeline instr_0
        next_pc_0 = shiftR <$> next_pc <*> 2
        output = bundle $ (readReg <$> regFile <*> 1) :> (readReg <$> regFile <*> 2) :> (readReg <$> regFile <*> 3) :> (readReg <$> regFile <*> 4) :> (readReg <$> regFile <*> 5) :> (readReg <$> regFile <*> 6) :> (readReg <$> regFile <*> 7) :> (readReg <$> regFile <*> 8) :> (readReg <$> regFile <*> 9) :> (readReg <$> regFile <*> 10) :> Nil

test n = P.head $ P.reverse $ sampleN n $ cpuHardware (func_ICache ++ repeat 0)

topEntity :: Clock System Source -> Reset System Asynchronous -> Signal System (Vec 10 XTYPE)
topEntity clk rst = withClockReset clk rst $ cpuHardware (func_ICache ++ repeat 0)

simpleProgram = 
    Itype   ADD (Register 0) (Register 1) 5               :>        -- Fails without forwarding       
    Itype   ADD (Register 0) (Register 2) 6               :>
    Itype   ADD (Register 0) (Register 2) 6               :>
    Rtype   ADD (Register 0) (Register 2) (Register 3) 0  :>
    Nil

branchProgram = 
    Itype   ADD (Register 0) (Register 1) 4               :>
    Itype   ADD (Register 0) (Register 2) 5               :>
    Branch  BEQ (Register 0) (Register 0) 4               :>
    Itype   ADD (Register 0) (Register 3) 6               :>
    Itype   ADD (Register 0) (Register 4) 7               :>
    Nil
    

simple_ICache = 
    0b00000000000100000000000010010011 :>
    0b00000000001000000000000100010011 :>
    0b00000000001100000000000110010011 :>
    0b00000000010100000000001000010011 :>
    0b00000000100000000000001010010011 :>
    0b00000000110000000000001100010011 :>
    0b00000000111100000000001110010011 :>
    0b00000000001000001000010000110011 :>
    0b00000000001001001000010010110011 :>
    Nil

simple_br_ICache = 
    0b00000000000100000000000010010011 :>
    0b00000000001000000000000100010011 :>
    0b00000010000000001000000001100011 :>
    0b00000000001100000000000110010011 :>
    0b00000000010000000000001000010011 :>
    0b00000000010100000000001010010011 :>
    0b00000000000000000000100001100011 :>
    0b00000000011000000000001100010011 :>
    0b00000000011100000000001110010011 :>
    0b00000000100000000000010000010011 :>
    0b00000000100100000000010010010011 :>
    0b00000000000000000000000001100011 :>
    Nil

fib_ICache = 
    0b00000000000100000000000010010011 :>
    0b00000000000000000000000100110011 :>
    0b00000000000100000000000110010011 :>
    0b00000000011000000000001010010011 :>
    0b00000000010100011010001100110011 :>
    0b00000000000000110000110001100011 :>
    0b00000000000100010000001000110011 :>
    0b00000000000000010000000010110011 :>
    0b00000000000000100000000100110011 :>
    0b00000000000100011000000110010011 :>
    0b11111110000000000000010011100011 :>
    0b00000000000000000000000001100011 :>
    Nil

simple_jal_ICache = 
    0b00000000000100000000000010010011 :>
    0b00000000001000000000000100010011 :>
    0b00000001000000000000000011101111 :>
    0b00000000001100000000000110010011 :>
    0b00000000010000000000001000010011 :>
    0b00000000010100000000001010010011 :>
    0b00000001000000000000010011101111 :>
    0b00000000011000000000001100010011 :>
    0b00000000011100000000001110010011 :>
    0b00000000100000000000010000010011 :>
    0b00000000100100000000010010010011 :>
    0b00000000000000000000000001101111 :>
    Nil

func_ICache = 
    0b00000000101000000000000010010011 :>
    0b00000001100100000000000100010011 :>
    0b00000000110000000000001011101111 :>
    0b00000000001000001000001000110011 :>
    0b00000000000000000000000001101111 :>
    0b00000000001000001000000110110011 :>
    0b00000000000000101000000001100111 :>
    Nil

simpleProgMem = encodeInstruction <$> simpleProgram  
branchProgMem = encodeInstruction <$> branchProgram  