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

topEntity clk rst initialState program = withClockReset clk rst $ cpuHardware initialState program

-- simpleProgram = 
--     Itype   ADD (Register 0) (Register 1) 5               :>              -- R1 = n = 5       
--     Itype   ADD (Register 0) (Register 2) 1               :>              -- R2 = i: current natural
--     Rtype   ADD (Register 0) (Register 0) (Register 3) 0  :>              -- R3 = s: current sum

--     Rtype   ADD (Register 3) (Register 2) (Register 3) 0  :>              -- jump target
--     Itype   ADD (Register 2) (Register 2) 1               :>
--     Itype   ADD (Register 1) (Register 1) (-1)            :>
--     Branch  BNE (Register 1) (Register 0) (-8)            :>              -- -8 * 2 (required by specification to enforce multiple of 2) = 16 = 4 insts * 4 (pc is incremented)
--     Nil

-- simpleProgramMem :: Vec 16 XTYPE
-- simpleProgramMem = fmap encodeInstruction simpleProgram ++ repeat 0

-- defaultCPUState = CPUState Fetch (Registers { general = repeat 0, pc = 0})

-- simulation = fmap (\(pc, x) -> (unpack pc :: XSigned, fmap unpack x :: [XSigned])) $ catMaybes $ sampleN 92 $ cpuHardware defaultCPUState simpleProgramMem