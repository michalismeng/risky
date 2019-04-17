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

import Clash.Prelude

import Control.DeepSeq (NFData)

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
    0b11111110100111111111000001101111 :>
    0b00000000000000000000000001101111 :>
    Nil
fib_DCache = 
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
func_DCache = 
    Nil

ld_hb_ICache = 
    0b00000000000000000000000010010011 :>
    0b00000000000000001001000100000011 :>
    0b00000000010000001001000110000011 :>
    0b00000000010000001000000010010011 :>
    0b00000000000000001000001000000011 :>
    0b00000000000100001000001010000011 :>
    0b00000000000000000000000001100011 :>
    Nil
ld_hb_DCache = 
    0b00010010001101000001001000110100 :>
    0b01000101011001110100010101100111 :>
    0b00000000000000000000000000000000 :>
    0b00000000000000000000000000000000 :>
    0b00000000000000000000000000000000 :>
    0b00000000000000000000000000000000 :>
    0b00000000000000000000000000000000 :>
    0b00000000000000000000000000000000 :>
    0b00000000000000000000000000000000 :>
    0b00000000000000000000000000000000 :>
    0b00000000000000000000000000000000 :>
    0b00000000000000000000000000000000 :>
    0b00000000000000000000000000000000 :>
    0b00000000000000000000000000000000 :>
    0b00000000000000000000000000000000 :>
    0b00000000000000000000000000000000 :>
    Nil

cpuHardware initialCPU (initialProg :: Vec 32 XTYPE) (initialData :: Vec 16 XTYPE) = output
    where
        state = register (initialCPU, initialProg, initialData) state'
        state' = fmap Core.Pipeline.cycle state

        output = fmap getOutput state
        getOutput (CPUState s regs, _, _) = case s of
            Fetch -> (1, (readRegister regs PC), readRegister regs (Register 5))
            _     -> (0, 0, 0)

topEntity :: Clock System Source -> Reset System Asynchronous -> Signal System (BitVector 3, XTYPE, XTYPE)
topEntity clk rst = withClockReset clk rst $ cpuHardware defaultCPUState (ld_hb_ICache ++ repeat 0) (ld_hb_DCache ++ repeat 0)

simpleProgram = 
    Itype   ADD (Register 0) (Register 1) 5               :>              -- R1 = n = 5       
    Itype   ADD (Register 0) (Register 2) 1               :>              -- R2 = i: current natural
    Rtype   ADD (Register 0) (Register 0) (Register 3) 0  :>              -- R3 = s: current sum

    Rtype   ADD (Register 3) (Register 2) (Register 3) 0  :>              -- jump target
    Itype   ADD (Register 2) (Register 2) 1               :>
    Itype   ADD (Register 1) (Register 1) (-1)            :>
    Branch  BNE (Register 1) (Register 0) (-8)            :>              -- -8 * 2 (required by specification to enforce multiple of 2) = 16 = 4 insts * 4 (pc is incremented)
    Store   SB  (Register 0) (Register 3) 0               :>
    Store   SH  (Register 0) (Register 3) 4               :>
    Store   SW  (Register 0) (Register 3) 8               :>
    
    Branch  BEQ (Register 0) (Register 0) (-2)            :>
    Nil


defaultCPUState = CPUState Fetch (Registers { general = repeat 0, pc = 0})

-- simHardware :: Clock System Source -> Reset System Asynchronous -> Signal System (BitVector 3, XTYPE, XTYPE)
-- simHardware clk rst = withClockReset clk rst $ cpuHardware defaultCPUState simpleProgramMem

-- simulation = sampleN 100 $ fmap translate $ cpuHardware defaultCPUState simpleProgramMem defaultDCache
--     where
--         translate (x, y, z) = (x, unpack y :: XSigned, unpack z :: XSigned)