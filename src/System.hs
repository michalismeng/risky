module System where

import Core.RegFile
import Core.Decode
import Core.Definitions
import Core.Pipeline

import Out.Func
import Out.Simple_lui
import Out.Ld_word

import Data.Bool
import Data.Maybe (catMaybes)

import Clash.Prelude

import qualified Prelude as P

import Control.DeepSeq (NFData)

-- Hack from https://github.com/adamwalker/clash-riscv
-- fixes ROM initial value ? 
firstCycleDef' :: HiddenClockReset dom sync gated => a -> Signal dom a -> Signal dom a
firstCycleDef' defa = mealy step False
    where
    step False _ = (True, defa)
    step True  x = (True, x)

firstCycleDef :: (HiddenClockReset dom sync gated, Default a) => Signal dom a -> Signal dom a
firstCycleDef = firstCycleDef' def
-----

cpuHardware :: HiddenClockReset dom gated sync => Vec (2 ^ 5) (BitVector 32) -> Vec (2 ^ 6) (BitVector 32) -> Signal dom (Vec 10 XTYPE)
cpuHardware initialProg initialMem = output
    where
        instruction = firstCycleDef $ romPow2 initialProg (unpack . resize <$> next_pc_0)

        memBaseAddr = 0x400000
        writeAddr' = shiftR <$> (writeAddr - memBaseAddr) <*> 2
        write = mux writeEnable (Just <$> bundle ((unpack . resize) <$> writeAddr', writeValue)) (pure Nothing)

        memData = blockRamPow2 initialMem ((unpack . resize) <$> readAddr) write

        (regFile, next_pc, readAddr, writeAddr, writeValue, writeEnable) = pipeline instruction memData
        next_pc_0 = shiftR <$> next_pc <*> 2
        output = bundle $ (readReg <$> regFile <*> 1) :> (readReg <$> regFile <*> 2)  :> 
                          (readReg <$> regFile <*> 3) :> (readReg <$> regFile <*> 4)  :> 
                          (readReg <$> regFile <*> 5) :> (readReg <$> regFile <*> 6)  :> 
                          (readReg <$> regFile <*> 7) :> (readReg <$> regFile <*> 8)  :> 
                          (readReg <$> regFile <*> 9) :> (readReg <$> regFile <*> 10) :> Nil

topEntity :: Clock System Source -> Reset System Asynchronous -> Signal System (Vec 10 XTYPE)
topEntity clk rst = withClockReset clk rst $ cpuHardware (ld_word_ICache ++ repeat 0) (ld_word_DCache ++ repeat 0)

-- adjustReadBlockRAM :: XTYPE -> Unsigned 5
adjustBlockRAM readAddr = unpack $ slice d4 d0 readAddr