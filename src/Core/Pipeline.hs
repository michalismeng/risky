module Core.Pipeline where

import Core.RegFile
import Core.Fetch
import Core.Decode
import Core.Execute
import Core.Memory
import Core.Writeback
import Core.Definitions

import Clash.Prelude

cycle (CPUState state registers, icache, dcache) = case state of
    Fetch -> (CPUState state' registers', icache, dcache) 
        where 
            state' = Decode (fetchInstruction registers icache)
            registers' = writeRegister registers PC (pc registers + 4)

    Decode instr -> (CPUState state' registers, icache, dcache) 
        where 
            instrD = decodeInstruction instr
            state' = Execute (decodeInstructionE registers instrD)

    Execute instrE -> (CPUState state' registers, icache, dcache) 
        where
            result = execute instrE
            state' = Memory result
    
    Memory memResult -> (CPUState state' registers, icache, dcache')
        where
            (dcache', result) = memory dcache memResult 
            state' = WriteBack result
            
    WriteBack result -> (CPUState state' registers', icache, dcache)
        where
            registers' = writeback registers result
            state' = Fetch

cpuHardware initialCPU initialProg initialData = output
    where
        state = register (initialCPU, initialProg, initialData) state'
        state' = fmap Core.Pipeline.cycle state

        output = fmap getOutput state'
        getOutput (CPUState s regs, _, _) = case s of
            Fetch -> (1, (readRegister regs PC - 4), (readRegister regs (Register 3) ))
            Decode _ -> (2, (readRegister regs PC - 4), (readRegister regs (Register 3) )) 
            Execute _ -> (3, (readRegister regs PC - 4), (readRegister regs (Register 3) ))
            Memory _   -> (4, (readRegister regs PC - 4), (readRegister regs (Register 3) ))
            WriteBack _ -> (5, (readRegister regs PC - 4), (readRegister regs (Register 3) ))