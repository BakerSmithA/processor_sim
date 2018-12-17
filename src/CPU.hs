module CPU where

import Control.Monad (foldM)
import State as St
import Instr
import Pipeline as P
import qualified Mem as Mem
import WriteBack
import Decode
import Types
import ExecUnit
import qualified RS
import Debug.Trace

-- Removes any instructions that occur after a branch.
stopAtBranch :: [FInstr] -> [FInstr]
stopAtBranch [] = []
stopAtBranch (i:is) | isBranch i = [i]
                    | otherwise  = i:(stopAtBranch is)

-- Fetches the number of instructions specified, stopping at the end of
-- instructions, or if a branch is encountered (the branch will be included in
-- returned instrs).
fetchN :: Addr -> Addr -> State -> [FInstr]
fetchN n start st = stopAtBranch $ Mem.take n start (instrs st)

-- Fetches a number of instructions, starting at the instruction pointed to by
-- the program counter.
fetch :: State -> Res [FInstr]
fetch st = do
     pc <- St.pcVal st
     let n = St.numFetch st
     return $ fetchN n (fromIntegral pc) st

-- Places executed results in reorder buffer.
commit :: [(WriteBack, ROBIdx, FreedReg)] -> State -> Res State
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
    st3 <- foldM writeBackFreed st2 is
    -- Only increment the number of instructions executed if any were.
    let st4 = if is /= [] then St.incExec (length is) st3 else st3
    return st4

-- Writes the result of an instruction back to memory/register file, and
-- invalidates the physical register previously mapped.
writeBackFreed :: State -> (WriteBack, FreedReg) -> Res State
writeBackFreed st1 (wb, freed) = do
    st2 <- writeBackSingle wb st1
    St.clearFreedReg freed st2

-- Writes the result of an instruction back to memory/register file.
writeBackSingle :: WriteBack -> State -> Res State
writeBackSingle (WriteReg r val)  st = CPU.setRegVal r val st
writeBackSingle (WriteMem i val)  st = St.setMemVal i val st
writeBackSingle (WritePrint s)    st = addOutput s st
writeBackSingle (NoOp)            st = return st
writeBackSingle (Terminate)       st = Exit st

-- Return whether the pipeline should stall to wait for branch instructions
-- to be executed, i.e. if there are branch instructions in the fetch or decode
-- stages. Do not need to check for execute stage because write-back results
-- are available via bypass.
shouldStall :: State -> Pipeline -> Bool
shouldStall st p = f || d || rs where
    f       = any isBranch (fetched p)
    d       = any isBranch (fmap pipeInstr (decoded p))
    rs      = not (RS.isEmpty (bRS st))

-- Shifts instructions through pipeline.
advancePipeline :: [FInstr] -> State -> Pipeline -> Res (State, Pipeline)
advancePipeline fetched st1 p = do
    (st2, p') <- P.advance (fetched, st1) decode exec CPU.commit writeBack p
    return (st2, p')

-- Shift instructions through pipeline, fetching a new instruction on each cycle.
cycle :: State -> Pipeline -> Res (State, Pipeline)
cycle st1 p = do
    fetched <- fetch st1
    (st2, p') <- advancePipeline fetched st1 p
    st3 <- St.incPC (fromIntegral $ length fetched) st2
    return (st3, p')

-- Shift instructions through pipeline without fetching a new instruction.
-- PC is also NOT updated.
cycleStall :: State -> Pipeline -> Res (State, Pipeline)
cycleStall st1 p = do
    (st2, p') <- advancePipeline [] st1 p
    return (st2, p')

-- Run processor to completion, i.e. until exit system call occurs.
runPipeline :: State -> Pipeline -> Res (State, Pipeline)
runPipeline st p = do
    let x = if not (shouldStall st p)
                then CPU.cycle st p
                else CPU.cycleStall st p
    (st', p') <- x
    -- runPipeline (St.incCycles st') p'
    trace (show p ++ "\n" ++ debugShow st ++ "\n====\n") $ runPipeline (St.incCycles st') p'

-- Run Res to completion starting with an empty pipeline.
run :: State -> State
run st =
    case runPipeline st P.empty of
        Exit st    -> st
        Crash e st -> error (show e ++ "\n" ++ debugShow st)
        Res x      -> error ("Did not terminate:" ++ show x)
