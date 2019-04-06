module Core.Writeback where

import Core.Definitions
import Core.RegFile
import Clash.Prelude

writeback :: Registers -> Result -> Registers
writeback registers result = case result of
    ChangeReg val reg -> writeRegister registers reg $ pack val