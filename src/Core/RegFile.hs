
module Core.RegFile where

import Core.Definitions 
import Clash.Sized.Index
import Clash.Sized.Vector (Vec)
import Clash.Sized.Vector ((!!), replace)
import Clash.Sized.BitVector (BitVector)

readRegister :: Registers -> Register -> XTYPE
readRegister registers reg = case reg of
    Register 0 -> 0
    Register i -> (general registers) !! i 
    PC         -> (pc registers)

writeRegister :: Registers -> Register -> XTYPE -> Registers
writeRegister registers reg value = case reg of
    Register 0 -> registers
    Register i -> registers { general = general' } where general' = replace i value (general registers)
    PC         -> registers { pc = value }