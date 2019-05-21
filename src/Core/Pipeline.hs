{-# LANGUAGE DeriveGeneric, DeriveAnyClass, ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module Core.Pipeline where

import Core.RegFile
import Core.Fetch
import Core.Decode
import Core.Execute
import Core.Memory
import Core.Writeback
import Core.Definitions

import qualified Data.Text as T
import Data.Monoid

import GHC.Generics
import Clash.Prelude

cycle (CPUState state registers, icache, dcache) = case state of
    Fetch -> (CPUState state' registers', icache, dcache) 
        where 
            state' = Decode (fetchInstruction registers icache)
            registers' = writeRegister registers PC (pc registers + 4)

    Decode instr -> (CPUState state' registers, icache, dcache) 
        where 
            instrD = decodeInstruction instr
            state' = Execute (decodeInstructionE registers instrD)

    Execute instrE -> (CPUState state' registers, icache, dcache) 
        where
            result = execute instrE
            state' = Memory result
    
    Memory memResult -> (CPUState state' registers, icache, dcache')
        where
            (dcache', result) = memory dcache memResult 
            state' = WriteBack result
            
    WriteBack result -> (CPUState state' registers', icache, dcache)
        where
            registers' = writeback registers result
            state' = Fetch

data ForwardingStage 
    = FwEx
    | FwMem
    | FwNone

instructionFetch instruction branchInstr controlTransfer_1 controlTransfer_2 controlTarget_2 = (pc, next_pc, instr)
    where
        pc = register (-4) next_pc
        next_pc = calcNextPC <$> pc <*> branchInstr <*> controlTransfer_1 <*> controlTransfer_2 <*> controlTarget_2
        instr = mux (controlTransfer_1 .||. controlTransfer_2) 0 instruction

        -- When jumping, current pc points to the next instruction (assume not taken branch)
        -- output NOP for one cycle -> branch instruction then enters execute stage
        -- if branch is taken -> NOP for another cycle (the current one) since the fetched instruction is bad (assumed not taken branch)

        calcNextPC curPC instr2 ctl_1 ctl_2 trg = case (ctl_1, ctl_2) of
            (False, True)  -> curPC - 4 + trg
            (False, False) -> curPC + 4
            (True, _)      -> curPC

instructionDecode :: Signal dom XTYPE -> Signal dom XTYPE -> Signal dom XTYPE -> Signal dom (Vec 32 (BitVector 32))
    -> (Signal dom XTYPE, Signal dom XTYPE, Signal dom Bool, Signal dom Bool, Signal dom Bool, Signal dom Bool, Signal dom ForwardingStage, Signal dom ForwardingStage)
instructionDecode instruction nextInstruction nnextInstruction regFile = (rs1Data, rs2Data, controlTransfer, shouldStall, alu1Register, alu2Register, fwdRs1, fwdRs2)
    where 
        rs1Addr = rs1 <$> instruction
        rs2Addr = rs2 <$> instruction
        rdAddr  = rd  <$> instruction

        rs1Data = readReg <$> regFile <*> rs1Addr' where rs1Addr' = unpack <$> rs1Addr
        rs2Data = readReg <$> regFile <*> rs2Addr' where rs2Addr' = unpack <$> rs2Addr

        controlTransfer = jal <$> instruction .||. jalR <$> instruction .||. branch <$> instruction

        -- stall when the next instruction is a load with the same destination register as one of the source registers of this instruction
        shouldStall = (load <$> nextInstruction)                             .&&.
                    (((rs1Addr .==. rdAddr) .&&. (usesRs1 <$> instruction) ) .||. 
                     ((rs2Addr .==. rdAddr) .&&. (usesRs2 <$> instruction) )) 

        alu1Register = usesRs1 <$> instruction
        alu2Register = usesRs2 <$> instruction

        fwdRs1 = caclForwardingStage <$> rs1Addr' <*> nextInstruction <*> nnextInstruction where rs1Addr' = unpack <$> rs1Addr
        fwdRs2 = caclForwardingStage <$> rs2Addr' <*> nextInstruction <*> nnextInstruction where rs2Addr' = unpack <$> rs2Addr

        caclForwardingStage :: Index 32 -> XTYPE -> XTYPE -> ForwardingStage
        caclForwardingStage rsAddr instrEx instrMem
            | rsAddr == 0                                       = FwNone
            | unpack (rd instrEx)  == rsAddr && usesRd instrEx  = FwEx
            | unpack (rd instrMem) == rsAddr && usesRd instrMem = FwMem
            | otherwise                                         = FwNone

instructionExecute :: Signal dom XTYPE
    -> Signal dom (BitVector 32)
    -> Signal dom (BitVector 32)
    -> Signal dom (BitVector 32)
    -> Signal dom Bool
    -> Signal dom Bool
    -> Signal dom ForwardingStage
    -> Signal dom ForwardingStage
    -> Signal dom (BitVector 32)
    -> Signal dom (BitVector 32)
    -> (Signal dom (BitVector 32), Signal dom Bool, Signal dom Bool)
instructionExecute instruction pc2 rs1Data rs2Data aluUsesRs1 aluUsesRs2 fwType1 fwType2 fwMem fwWB = (aluRes, bruRes, writesToRegFile)
    where
        aluOpcode = decodeAluOpcode <$> instruction     --TODO: perhaps move these to ID stage
        bruOpcode = decodeBruOpcode <$> instruction
        writesToRegFile = usesRd <$> instruction
        
        immData = (resize . iImm) <$> instruction
        effectiveRs1 = fwMux <$> fwType1 <*> rs1Data <*> fwMem <*> fwWB
        effectiveRs2 = fwMux <$> fwType2 <*> rs2Data <*> fwMem <*> fwWB

        aluOperand1 = mux aluUsesRs1 effectiveRs1 pc2
        aluOperand2 = mux aluUsesRs2 effectiveRs2 immData
        
        aluRes = alu2 <$> aluOpcode <*> aluOperand1 <*> aluOperand2
        bruRes = bru2 <$> bruOpcode <*> aluOperand1 <*> aluOperand2

        fwMux fwType rs ex mem = case fwType of
            FwNone  -> rs
            FwEx    -> ex
            FwMem   -> mem

pipeline 
    :: forall dom sync gated. HiddenClockReset dom gated sync
    => Signal dom XTYPE 
    -> (Signal dom (Vec 32 XTYPE), Signal dom XTYPE)
pipeline fromInstructionMem = (theRegFile, next_pc_0)
    where
        -- Stage 0
        (pc_0, next_pc_0, instr_0) = instructionFetch fromInstructionMem instr_2 controlTransfer_1 controlTransfer_2 controlTarget_2

        -- Stage 1
        pc_1 = register 0 $ mux shouldStall pc_1 pc_0
        instr_1 = register 0 $ mux shouldStall 0 instr_0
        theRegFile = regFile rdAddr_4 regWriteEnable_4 rdData_4  -- we write first the result of WB and then read rs1 and rs2

        (rs1Data_1, rs2Data_1, controlTransfer_1, shouldStall, aluUsesRs1_1, aluUsesRs2_1, fwdRs1_1, fwdRs2_2) 
                = instructionDecode instr_1 instr_2 instr_3 theRegFile

        -- Stage 2
        pc_2    = register 0 pc_1
        instr_2 = register 0 $ mux shouldStall 0 instr_1
        rs1Data_2 = register 0 rs1Data_1
        rs2Data_2 = register 0 rs2Data_1
        aluUsesRs1_2 = register False aluUsesRs1_1 
        aluUsesRs2_2 = register False aluUsesRs2_1 
        forwardRs1_2 = register FwNone fwdRs1_1
        forwardRs2_2 = register FwNone fwdRs2_2
        ctlTransf_2 = register False controlTransfer_1
        controlTransfer_2 = ctlTransf_2 .&&. (bruRes_2 .||. jal <$> instr_2 .||. jalR <$> instr_2)

        branchTarget_2 = shiftL <$> ((signExtend . bImm) <$> instr_2) <*> 1
        jalTarget_2    = shiftL <$> ((signExtend . jImm) <$> instr_2) <*> 1

        calcControlTarget instr bTarget jTarget jrTarget pc
            | branch instr  = bTarget
            | jal instr     = jTarget
            | jalR instr    = jrTarget - pc
            | otherwise     = 0

        calcExecRes instr aluRes pcNext
            | jal instr || jalR instr = pcNext
            | otherwise = aluRes
           
        (aluRes_2, bruRes_2, writesToRegFile_2) = 
            instructionExecute instr_2 pc_2 rs1Data_2 rs2Data_2 aluUsesRs1_2 aluUsesRs2_2 forwardRs1_2 forwardRs2_2 execRes_3 rdData_4  
                
        execRes_2 = calcExecRes <$> instr_2 <*> aluRes_2 <*> (pc_2 + 4)
        controlTarget_2 = calcControlTarget <$> instr_2 <*> branchTarget_2 <*> jalTarget_2 <*> aluRes_2 <*> pc_2

        -- Stage 3
        pc_3 = register 0 pc_2
        instr_3 = register 0 instr_2
        execRes_3 = register 0 execRes_2
        regWriteEnable_3 = register False writesToRegFile_2

        -- Stage 4
        pc_4 = register 0 pc_3
        instr_4 = register 0 instr_3
        execRes_4 = register 0 execRes_3
        rdAddr_4 = (unpack . rd) <$> instr_4
        regWriteEnable_4 = register False regWriteEnable_3

        rdData_4 = execRes_4
