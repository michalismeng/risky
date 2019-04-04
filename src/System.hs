module System where

import Core.RegFile
import Core.Decode
import Core.Execute
import Core.Definitions
import Data.Bool

import Prelude ()
import Clash.Prelude

registers = Registers { general = repeat (-1) :: Vec 32 XTYPE, pc = 0 :: XTYPE}

topEntity = execute decE
    where
        x = encodeInstruction $ Itype SRL (Register 1) (Register 2) 0b000000000001
        decD = decodeInstruction x
        decE = decodeInstructionE registers decD

testLui = encodeInstruction $ Utype AUIPC (Register 2) 25
testLuiDec = decodeInstruction testLui

testJalR = encodeInstruction $ Itype JALR (Register 1) (Register 2) 26
testJalRDec = decodeInstruction testJalR
testJalRDecE = decodeInstructionE registers testJalRDec