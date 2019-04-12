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

topEntity :: Clock System Source -> Reset System Asynchronous -> Signal System (BitVector 3, XTYPE, XTYPE)
topEntity clk rst = withClockReset clk rst $ cpuHardware defaultCPUState simpleProgramMem defaultDCache

simpleProgram = 
    Itype   ADD (Register 0) (Register 1) 5               :>              -- R1 = n = 5       
    Itype   ADD (Register 0) (Register 2) 1               :>              -- R2 = i: current natural
    Rtype   ADD (Register 0) (Register 0) (Register 3) 0  :>              -- R3 = s: current sum

    Rtype   ADD (Register 3) (Register 2) (Register 3) 0  :>              -- jump target
    Itype   ADD (Register 2) (Register 2) 1               :>
    Itype   ADD (Register 1) (Register 1) (-1)            :>
    Branch  BNE (Register 1) (Register 0) (-8)            :>              -- -8 * 2 (required by specification to enforce multiple of 2) = 16 = 4 insts * 4 (pc is incremented)
    Branch  BEQ (Register 0) (Register 0) (-2)            :>
    Nil

-- simpleProgramMem' :: Vec 16 XTYPE            
-- simpleProgramMem' = fmap encodeInstruction simpleProgram ++ repeat 0     -- ! this function causes a lot of compilation trouble (memory + time) !!!

simpleProgramMem :: Vec 16 XTYPE
simpleProgramMem = 0b00000000010100000000000010010011 :> 
                   0b00000000000100000000000100010011 :> 
                   0b00000000000000000000000110110011 :> 
                   0b00000000001000011000000110110011 :> 
                   0b00000000000100010000000100010011 :> 
                   0b11111111111100001000000010010011 :> 
                   0b11111110000000001001100011100011 :> Nil ++ repeat 0

defaultDCache :: Vec 16 XTYPE
defaultDCache = repeat 0

defaultCPUState = CPUState Fetch (Registers { general = repeat 0, pc = 0})

-- simHardware :: Clock System Source -> Reset System Asynchronous -> Signal System (BitVector 3, XTYPE, XTYPE)
-- simHardware clk rst = withClockReset clk rst $ cpuHardware defaultCPUState simpleProgramMem

simulation = sampleN 100 $ fmap translate $ cpuHardware defaultCPUState simpleProgramMem defaultDCache
    where
        translate (x, y, z) = (x, unpack y :: XSigned, unpack z :: XSigned)