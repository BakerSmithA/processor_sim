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
import RRT (RegMap(..))
import qualified ROB
import Debug.Trace

-- Removes any instructions that occur after a branch.
stopAtBranch :: [FPipeInstr] -> [FPipeInstr]
stopAtBranch [] = []
stopAtBranch (i:is) | isBranch (fst i) = [i]
                    | otherwise        = i:(stopAtBranch is)

-- Fetches the number of instructions specified, stopping at the end of
-- instructions, or if a branch is encountered (the branch will be included in
-- returned instrs).
fetchN :: Addr -> Addr -> State -> [FPipeInstr]
fetchN n start st = stopAtBranch $ map f (Mem.take n start (instrs st)) where
    f (i, savedPC) = (i, fromIntegral savedPC)

-- Fetches a number of instructions, starting at the instruction pointed to by
-- the program counter.
fetch :: State -> Res [FPipeInstr]
fetch st = do
     pc <- St.pcVal st
     let n = trace ("FETCH AT: " ++ show pc) St.numFetch st
     return $ fetchN n (fromIntegral pc) st

-- Places executed results in reorder buffer.
commit :: [PipeData WriteBack] -> State -> Res State
commit wbs st = return (St.addROB st wbs)

-- Set the value stored in a register, or Crash if invalid index.
setRegVal :: PhyReg -> Val -> State -> Res State
setRegVal i val = St.setRegVal i (Just val)

-- Add string to output of Res.
addOutput :: String -> State -> Res State
addOutput s st = return st { output = (output st) ++ s }

-- Perform write-back stage of pipeline, writing result back to register/memory.
writeBack :: State -> Res (State, ShouldFlush)
writeBack st1 = do
    let st2                = CPU.invalidateLoads st1
        (is, st3)          = St.commitROB st2
        (validIs, savedPC) = validWriteBacks is
        (wbs, frees, maps) = split validIs
        st4                = setRRTMappings maps st3

    st6 <- writeBackInstrs wbs st4
    st7 <- invalidateRegs frees st6

    -- Whether to flush the pipeline.
    case savedPC of
        Nothing -> return (st7, NoFlush)
        Just pc -> trace ("FLUSH: " ++ show pc) $ do
            st8 <- St.flush pc st7
            return (st8, Flush)

-- Invalidates loads in the ROB if the next writeback instruction to be committed
-- is a memory write that has a clashing address.
invalidateLoads :: State -> State
invalidateLoads st =
    case ROB.peek (rob st) of
        Nothing         -> st
        Just (wb, _, _) ->
            case wb of
                WriteMem a _ -> St.invalidateLoads a st
                _            -> st

-- Return all writeback instructions up to an invalid load, if there is one
-- in the supplied list. Returns whether there was an invalid load, and whether
-- the pipeline should be flushed to the returned PC value.
validWriteBacks :: [(WriteBack, FreedReg, SavedPC, RegMap)] -> ([(WriteBack, FreedReg, RegMap)], Maybe SavedPC)
validWriteBacks [] = ([], Nothing)
validWriteBacks ((wb, freed, savedPC, regMap):wbs) =
    case wb of
        WriteReg _ _ InvalidLoad -> ([], Just savedPC)
        _ -> ((wb, freed, regMap):wbs', shouldFlush) where
            (wbs', shouldFlush) = validWriteBacks wbs

split :: [(WriteBack, FreedReg, RegMap)] -> ([WriteBack], [FreedReg], [RegMap])
split = foldr f ([], [], []) where
    f (wb, freed, regMap) (wbs, frees, maps) = (wb:wbs, freed:frees, regMap:maps)

-- Adds mappings to RRT for committed instructions. We know these mappings
-- cannot change if there is a flush, therefore this is safe.
setRRTMappings :: [RegMap] -> State -> State
setRRTMappings ms st = foldr St.confirmRegMap st ms

-- Writes back instructions, assuming they are all valid.
writeBackInstrs :: [WriteBack] -> State -> Res State
writeBackInstrs wbs st = foldM write st wbs where
    write st (WriteReg r v _) = CPU.setRegVal r v st
    write st (WriteMem a v)   = St.setMemVal a v st
    write st (WritePrint s)   = addOutput s st
    write st (NoOp)           = return st
    write st (Terminate)      = Exit st

-- Invalidates the values stored in any registers after they have finished being used.
invalidateRegs :: [FreedReg] -> State -> Res State
invalidateRegs frees st = foldM f st frees where
    f st' freed = St.freeReg freed st'

-- Return whether the pipeline should stall to wait for branch instructions
-- to be executed, i.e. if there are branch instructions in the fetch or decode
-- stages. Do not need to check for execute stage because write-back results
-- are available via bypass.
shouldStall :: State -> Pipeline -> Bool
shouldStall st p = f || d || rs where
    f       = any isBranch (fmap fst (fetched p))
    d       = any isBranch (fmap pipeInstr (decoded p))
    rs      = not (RS.isEmpty (bRS st))

-- Shifts instructions through pipeline.
advancePipeline :: [FPipeInstr] -> State -> Pipeline -> Res (State, Pipeline, ShouldFlush)
advancePipeline fetched st1 p = P.advance (fetched, st1) decode exec CPU.commit writeBack p

-- Shift instructions through pipeline, fetching a new instruction on each cycle.
cycle :: State -> Pipeline -> Res (State, Pipeline)
cycle st1 p = do
    fetched <- fetch st1
    (st2, p', shouldFlush) <- advancePipeline fetched st1 p
    case shouldFlush of
        NoFlush -> do
            st3 <- St.incPC (fromIntegral $ length fetched) st2
            return (st3, p')
        Flush ->
            -- The PC is reset if a flush occurred. Therefore, don't increment.
            return (st2, p')

-- Shift instructions through pipeline without fetching a new instruction.
-- PC is also NOT updated.
cycleStall :: State -> Pipeline -> Res (State, Pipeline)
cycleStall st1 p = do
    (st2, p', _) <- advancePipeline [] st1 p
    return (st2, p')

-- Run processor to completion, i.e. until exit system call occurs.
runPipeline :: State -> Pipeline -> Res (State, Pipeline)
runPipeline st p = do
    let x = if not (shouldStall st p)
                then CPU.cycle st p
                else CPU.cycleStall st p
    (st', p') <- x
    -- runPipeline (St.incCycles st') p'
    trace (show p' ++ "\n" ++ debugShow st' ++ "\n====\n") $ runPipeline (St.incCycles st') p'

-- Run Res to completion starting with an empty pipeline.
run :: State -> State
run st =
    case runPipeline st P.empty of
        Exit st    -> st
        Crash e st -> error (show e ++ "\n" ++ debugShow st)
        Res x      -> error ("Did not terminate:" ++ show x)
