
module Core.RegFile where

import Core.Definitions 
import Clash.Sized.Index
import Clash.Sized.Vector (Vec)
import qualified Clash.Sized.Vector as V ((!!), replace)
import Clash.Sized.BitVector (BitVector)


readRegister registers reg = case reg of
    Register 0 -> 0
    Register i -> (general registers) V.!! i 
    PC         -> (pc registers)

writeRegister registers reg value = case reg of
    Register 0 -> registers
    Register i -> registers { general = general' } where general' = V.replace i value (general registers)
    PC         -> registers { pc = value }