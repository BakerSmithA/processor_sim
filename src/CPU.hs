module CPU where

import Control.Monad (foldM)
import State as St
import Instr
import Pipeline as P
import qualified Mem as Mem
import WriteBack
import Decode
import Types
import Exec
import qualified RS
import RRT (RegMap(..))
import qualified ROB
-- import Debug.Trace

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
     let n = St.numFetch st
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
writeBack :: State -> Res (State, ShouldFlush, Maybe Addr)
writeBack st1 = do
    let (st2, invalidAddr) = CPU.invalidateLoads st1
        (is, savedPC, st3) = St.commitROB st2
        (wbs, frees, maps) = split is
        st4                = setRRTMappings maps st3

    st5 <- writeBackInstrs wbs st4
    st6 <- invalidateRegs frees st5

    let st7 = St.incExec (length is) st6

    -- Whether to flush the pipeline.
    case savedPC of
        Nothing -> return (st7, NoFlush, invalidAddr)
        Just pc -> do
            -- Need to free any mapped registers still in the ROB otherwise
            -- they will be lost when the flush resets the ROB.
            let remainingFrees = ROB.mappedPhyRegs (rob st7)
            st8 <- St.flush pc remainingFrees st7
            return (st8, Flush, invalidAddr)

-- Invalidates loads in the ROB if the next writeback instruction to be committed
-- is a memory write that has a clashing address.
invalidateLoads :: State -> (State, Maybe Addr)
invalidateLoads st =
    case ROB.peek (rob st) of
        Nothing         -> (st, Nothing)
        Just (wb, _, _) ->
            case wb of
                WriteMem a _ -> (St.invalidateLoads a st, Just a)
                _            -> (st, Nothing)

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
setRRTMappings ms st = foldl f st ms where
    f st regMap = St.confirmRegMap regMap st

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
shouldStallFetch :: State -> Pipeline -> Bool
shouldStallFetch st p = f || d || rs || rob where
    f       = any isBranch (fmap fst (fetched p))
    d       = any isBranch (fmap pipeInstr (decoded p))
    rs      = not (RS.isEmpty (bRS st))
    rob     = False

shouldStallDecode :: State -> Bool
shouldStallDecode _ = False

shouldStallExec :: State -> Bool
shouldStallExec _ = False

shouldStallCommit :: State -> Bool
shouldStallCommit _ = False

-- Shifts instructions through pipeline.
advancePipeline :: [FPipeInstr] -> State -> Pipeline -> Res (State, Pipeline, ShouldFlush)
advancePipeline fetched st1 p =
    P.advance (fetched, st1) decode shouldStallDecode exec shouldStallExec CPU.commit shouldStallCommit writeBack p

-- Shift instructions through pipeline, fetching a new instruction on each cycle.
cycle :: State -> Pipeline -> Res (State, Pipeline)
cycle st1 p = do
    fetched <- fetch st1
    (st2, p', shouldFlush) <- advancePipeline fetched st1 p
    case shouldFlush of
        NoFlush -> do
            let n = fromIntegral $ length fetched
            st3 <- St.incPC n st2
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
    let x = if not (shouldStallFetch st p)
                then CPU.cycle st p
                else CPU.cycleStall st p
    (st', p') <- x
    runPipeline (St.incCycles st') p'
    -- trace (show p' ++ "\n" ++ debugShow st' ++ "\n====\n") $ runPipeline (St.incCycles st') p'

-- Run Res to completion starting with an empty pipeline.
run :: State -> State
run st =
    case runPipeline st P.empty of
        Exit st    -> st
        Crash e st -> error (show e ++ "\n" ++ debugShow st)
        Res x      -> error ("Did not terminate:" ++ show x)
