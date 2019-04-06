module Core.Fetch where

import Core.Definitions
import Clash.Prelude


fetchInstruction registers iCache = iCache !! pc'
    where
        pc' = shiftR (pc registers) 2
