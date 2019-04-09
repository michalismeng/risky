module Core.Pipeline where

import Core.RegFile
import Core.Fetch
import Core.Decode
import Core.Execute
import Core.Writeback
import Core.Definitions

import Clash.Prelude

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

cpuHardware initialCPU initialRAM = output      -- TODO: This function is for testing. Move to Spec.hs
    where
        state = register (initialCPU, initialRAM) state'
        state' = fmap Core.Pipeline.cycle state

        output = fmap getOutput state'
        getOutput (CPUState s regs, _) = case s of
            Fetch -> Just  ((readRegister regs PC - 4), (fmap (\i -> readRegister regs (Register i)) [0..5]))
            Decode _ -> Nothing 
            Execute _ -> Nothing
            WriteBack _ -> Nothing