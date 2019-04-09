module Core.Writeback where

import Core.Definitions
import Core.RegFile
import Clash.Prelude

import Debug.Trace

writeback :: Registers -> Result -> Registers
writeback registers result = case result of
    ChangeReg  val reg              -> writeRegister registers  reg $ pack val
    ChangeReg2 val1 reg1 val2 reg2  -> writeRegister registers' reg2 $ pack val2 where registers' = writeRegister registers reg1 $ pack val1