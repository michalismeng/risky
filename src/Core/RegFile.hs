
module Core.RegFile where

import Core.Definitions 
import Clash.Prelude

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

regFile 
    :: HiddenClockReset dom gated sync
    => Signal dom (Index 32)     --Write address
    -> Signal dom Bool           --Write enable
    -> Signal dom (BitVector 32) --Write data
    -> Signal dom (Vec 32 (BitVector 32))
regFile writeAddr writeEn writeData = file
    where
        file = mealy step (repeat 0) $ bundle (writeAddr, writeEn, writeData)
            where
                step regFile (writeAddr, writeEn, writeData) = (regFile', regFile')
                    where
                        regFile'
                            | writeEn = replace writeAddr writeData regFile
                            | otherwise = regFile

readReg :: Vec 32 (BitVector 32) -> Index 32 -> BitVector 32
readReg regFile idx
    | idx == 0 = 0
    | otherwise = regFile !! idx