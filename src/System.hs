module System where

import Core.RegFile
import Core.Fetch
import Core.Decode
import Core.Execute
import Core.Writeback
import Core.Definitions
import Data.Bool
import Data.Maybe (catMaybes)

import Prelude ()
import Clash.Prelude

import Control.DeepSeq (NFData)

simpleProgram = 
    Itype ADD (Register 0) (Register 1) 15 :>
    Itype ADD (Register 0) (Register 2) 20 :>
    Rtype ADD (Register 1) (Register 2) (Register 3) 0 :>
    Nil

simpleProgramMem :: Vec 16 XTYPE
simpleProgramMem = fmap encodeInstruction simpleProgram ++ repeat 0

defaultCPUState = CPUState Fetch (Registers { general = repeat 0, pc = 0})

cycle (CPUState state registers, icache) = case state of
    Fetch -> (CPUState state' registers, icache) 
        where 
            state' = Decode (fetchInstruction registers icache)

    Decode instr -> (CPUState state' registers, icache) 
        where 
            state' = Execute (decodeInstructionE registers instrD)
            instrD = decodeInstruction instr

    Execute instr -> (CPUState state' registers, icache) 
        where
            result = execute instr
            state' = WriteBack result 
            
    WriteBack result -> (CPUState state' registers_, icache)
        where 
            state' = Fetch
            registers' = writeback registers result
            registers_ = writeRegister registers' PC (pc registers' + 4)

cpuHardware initialCPU initialRAM = output
    where
        state = register (initialCPU, initialRAM) state'
        state' = fmap System.cycle state

        output = fmap getOutput state'
        getOutput (CPUState s regs, _) = case s of
            Fetch -> Just [readRegister regs (Register 1), readRegister regs (Register 2), readRegister regs (Register 3)]
            Decode _ -> Nothing 
            Execute _ -> Nothing
            WriteBack _ -> Nothing


topEntity = catMaybes $ sampleN 12 $ cpuHardware defaultCPUState simpleProgramMem