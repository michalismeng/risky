module System where

import Core.RegFile
import Core.Decode
import Core.Definitions
import Core.Pipeline

import Out.Func
import Out.Simple_lui

import Data.Bool
import Data.Maybe (catMaybes)

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
topEntity clk rst = withClockReset clk rst $ cpuHardware (simple_lui_ICache ++ repeat 0)

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

simpleProgMem = encodeInstruction <$> simpleProgram  
branchProgMem = encodeInstruction <$> branchProgram  