
module Core.Memory where

import Core.Definitions

import Clash.Prelude

readMemory dcache op address = case op of 
    LW  -> mem
    LH  -> unpack $ signExtend $ slice d15 d0 mem
    LHU -> unpack $ zeroExtend $ slice d15 d0 mem
    LB  -> unpack $ signExtend $ slice d7  d0 mem
    LBU -> unpack $ zeroExtend $ slice d7  d0 mem 
    where
        address' = shiftR address 2
        mem = dcache !! address'

writeMemory dcache op address value = case op of
    SW -> replace address' value dcache
    SH -> replace address' (slice d15 d0 value ++# mem16) dcache
    SB -> replace address' (slice d7  d0 value ++# mem24) dcache
    where
        address' = shiftR address 2
        mem = dcache !! address'
        mem16 = slice d15 d0 mem
        mem24 = slice d23 d0 mem

-- memory :: Vec 16 XTYPE -> MemoryResult -> (Vec 16 XTYPE, Result)
memory dcache memRes = case memRes of
    ReadMemory op base dst      -> (dcache, ChangeReg res dst)             where res = unpack $ readMemory dcache op base
    WriteMemory op base value   -> (dcache', ChangeReg 0 (Register 0))     where dcache' = writeMemory dcache op base (pack value)
    NoMemOp result              -> (dcache, result)                        -- unpack the executed result