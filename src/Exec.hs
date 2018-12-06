module Exec where

import Control.Monad (foldM)
import State as St
import Instr
import Pipeline as P
import qualified Mem as Mem
import qualified Bypass as BP
import WriteBack
import Decode
import Types
import ExecUnit

-- Perform fetch state of pipeline by retreiving instruction.
-- Or, return Nothing if the value of the pc is after the last instruction.
fetch :: State -> Res (Maybe FInstr, State)
fetch st = do
    pc <- St.pcVal st
    case Mem.load (fromIntegral pc) (instrs st) of
        Nothing -> return (Nothing, st)
        Just    instr -> return (Just instr, st)

-- Places executed results in reorder buffer.
commit :: [(WriteBack, ROBIdx)] -> State -> Res State
commit wbs st = return (St.addROB st wbs)

-- Set the value stored in a register, or Crash if invalid index.
setRegVal :: PhyReg -> Val -> State -> Res State
setRegVal i val = St.setRegVal i (Just val)

-- Add string to output of Res.
addOutput :: String -> State -> Res State
addOutput s st = return st { output = (output st) ++ s }

-- Perform write-back stage of pipeline, writing result back to register/memory.
writeBack :: State -> Res State
writeBack st1 = do
    let (is, st2) = St.commitROB st1
    st3 <- foldM writeBack' st2 is
    -- Only increment the number of instructions executed if any were.
    let st4 = if is /= [] then St.incExec st3 else st3
    return st4
        where
            writeBack' st (WriteReg r val) = Exec.setRegVal r val st
            writeBack' st (WriteMem i val) = St.setMemVal i val st
            writeBack' st (WritePrint s)   = addOutput s st
            writeBack' st (NoOp)           = return st
            writeBack' st (Terminate)      = Exit st

-- Increment PC by 1.
incPc :: State -> Res State
incPc st = do
    pc <- pcVal st
    pcReg <- St.namedReg pcIdx st
    Exec.setRegVal pcReg (pc+1) st

-- Return whether the pipeline should stall to wait for branch instructions
-- to be executed, i.e. if there are branch instructions in the fetch or decode
-- stages. Do not need to check for execute stage because write-back results
-- are available via bypass.
shouldStall :: Pipeline -> Bool
shouldStall p = f || d where
    f  = maybe False isBranch (fetched p)
    d  = maybe False isBranch (fmap fst (decoded p))

-- Shifts instructions through pipeline.
advancePipeline :: Maybe FInstr -> State -> Pipeline -> Res (State, Pipeline)
advancePipeline fetched st1 p = do
    -- TODO: Subsitute this with exec that updates state when RS implemented.
    -- let executer = \(di, idx) st -> fmap (\wb -> ([(wb, idx)], st)) (exec di st)
    (st2, p') <- P.advance (fetched, st1) decode exec Exec.commit writeBack p
    return (st2, p')

-- Returns state which contains bypass value that was just written as part of
-- the write-back stage of the pipeline. This makes this value available to
-- previous stages of the pipeline.
bypassed :: State -> Pipeline -> State
bypassed st p = St.withBypass b st where
    b = BP.fromWbs (fmap fst (executed p))

-- Shift instructions through pipeline, fetching a new instruction on each cycle.
cycle :: State -> Pipeline -> Res (State, Pipeline)
cycle st1 p = do
    (fetched, st2) <- fetch st1
    (st3, p') <- advancePipeline fetched st2 p
    st4 <- incPc st3
    return (bypassed st4 p', p')

-- Shift instructions through pipeline without fetching a new instruction.
-- PC is also NOT updated.
cycleStall :: State -> Pipeline -> Res (State, Pipeline)
cycleStall st1 p = do
    (st2, p') <- advancePipeline Nothing st1 p
    return (bypassed st2 p', p')

-- Run Res to completion, i.e. until exit system call occurs.
runPipeline :: State -> Pipeline -> Res (State, Pipeline)
runPipeline st p = do
    let x = if not (shouldStall p)
                then Exec.cycle st p
                else Exec.cycleStall st p
    (st', p') <- x
    runPipeline (St.incCycles st') p'

-- Run Res to completion starting with an empty pipeline.
run :: State -> State
run st =
    case runPipeline st P.empty of
        Exit st    -> st
        Crash e st -> error (show e ++ "\n" ++ show st)
        Res x      -> error ("Did not terminate:" ++ show x)
