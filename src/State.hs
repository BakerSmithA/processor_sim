module State where

import Control.Applicative
import Control.Monad (foldM)
import Data.Word (Word32)
import Mem (Mem)
import qualified Mem as Mem
import qualified Mem as Reg
import Instr
import Bypass (Bypass)
import qualified Bypass as BP
import Queue as Q
import ROB (ROB)
import qualified ROB as ROB
import Error
import WriteBack
import RRT
import RS (MemRS, ArithLogicRS, BranchRS, OutRS)
import RS (RS)
import qualified RS as RS
import Types
import ExecUnit as Unit

-- Stores current state of CPU at a point in time.
-- Uses Von Newmann architecture, and so data and instructions are separate.
data State = State {
    -- Stats
    -- Maximum number of instructions to fetch on each cycle.
    numFetch :: Word32

    -- Memory
  , mem    :: Mem Addr Val
    -- Register contains Nothing if it has not yet been written to, or it was
    -- invalidated when the physical regsiter was assigned to a new architectural
    -- register.
  , regs   :: Mem PhyReg (Maybe Val)
  , instrs :: Mem Addr FInstr

    -- Register indices
  , pcIdx  :: RegIdx -- Program Counter
  , spIdx  :: RegIdx -- Stack Pointer
  , lrIdx  :: RegIdx -- Link Register
  , bpIdx  :: RegIdx -- Base Pointer
  , retIdx :: RegIdx -- Return value register (EAX in x86)

    -- Output
  , output :: String

   -- Superscalar
  , bypass :: Bypass
  , rob    :: ROB
  , rrt    :: RRT

  , memRS    :: MemRS
  , memUnits :: [MemUnit]

  , alRS     :: ArithLogicRS
  , alUnits  :: [ALUnit]

  , bRS      :: BranchRS
  , bUnits   :: [BUnit]

  , outRS    :: OutRS
  , outUnits :: [OutUnit]

   -- Stats
  , cycles        :: Int
  , instrsExec    :: Int
  , flushes       :: Int
  , cyclesStalled :: Int

} deriving (Eq)

-- Current state of the virtual machine, or whether it crashed, e.g. by
-- accessing memory index that is out of bounds.
data Res a = Res a
           | Crash Error State
           | Exit State
           deriving (Eq, Show)

instance Functor Res where
    -- fmap :: (a -> b) -> f a -> f b
    fmap f (Res x)      = Res (f x)
    fmap _ (Crash e st) = Crash e st
    fmap _ (Exit st)    = Exit st

instance Applicative Res where
    -- pure :: a -> Res a
    pure = Res
    -- (<*>) :: f (a -> b) -> f a -> f b
    (Res f)      <*> vm = fmap f vm
    (Crash e st) <*> _ = Crash e st
    (Exit st)    <*> _ = Exit st

instance Monad Res where
    return = pure
    -- (>>=) :: m a -> (a -> m b) -> m b
    (Res x)      >>= f = f x
    (Crash e st) >>= _ = Crash e st
    (Exit st)    >>= _ = Exit st

instance Show State where
    show st =
        "\nInstrs         : "  ++ show (instrsExec st)
     ++ "\nCycles         : "  ++ show (cycles st)
     ++ "\nCycles Stalled : "  ++ show (cyclesStalled st)
     ++ "\nFlushes        : "  ++ show (flushes st)
     ++ "\nIpC            : "  ++ show (roundDp (ipc st) 2)
     ++ "\nIpC wo. stalls : "  ++ show (roundDp (ipcWithoutStalls st) 2)
     ++ "\n"
     ++ "\nReg            : "    ++ Mem.showNumbered (regs st)
     ++ "\nMem            :\n\n" ++ Mem.showBlocks 16 (mem st)

-- Instructions per cycle.
ipc :: State -> Double
ipc st = (fromIntegral $ instrsExec st) / (fromIntegral $ cycles st) :: Double

-- Predicted IpC if no cycles were stalled.
ipcWithoutStalls :: State -> Double
ipcWithoutStalls st = (fromIntegral $ (instrsExec st)) / (n :: Double) where
    n = (fromIntegral $ (cycles st) - (cyclesStalled st))

roundDp :: Double -> Int -> Double
roundDp x n = (fromInteger $ round $ x * (10^n)) / (10.0^^n)

debugShow :: State -> String
debugShow st =
        "\nBypass : "  ++ show (bypass st)
     ++ "\nRRT    : "  ++ show (rrt st)
     ++ "\nROB    :\n" ++ show (rob st)
     ++ "\nMem RS : "  ++ show (memRS st)
     ++ "\nAL  RS : "  ++ show (alRS st)
     ++ "\nB   RS : "  ++ show (bRS st)
     ++ "\nOut RS : "  ++ show (outRS st)
     ++ "\nReg    : "  ++ Mem.showNumbered (regs st)
     ++ "\nMem    :\n" ++ Mem.showBlocks 16 (mem st)

-- Defaults value of assigned physical registers to 0.
-- This is because there are registers they may be used without being
-- initialised, e.g. SP.
defaultedMem :: [Maybe Val] -> RRT -> [Maybe Val]
defaultedMem vals rrt = map f (zip [0..] vals) where
    f :: (PhyReg, Maybe Val) -> Maybe Val
    f (p, val) | isPhy p rrt = Just 0
               | otherwise   = val

-- Create state containing no values in memory or registers.
empty :: RegIdx -> RegIdx -> RegIdx -> RegIdx -> RegIdx -> [FInstr] -> State
empty pc sp lr bp ret instrs =
    State numFetch mem regs instrs' pc sp lr bp ret [] bypass rob rrt memRS memUnits alRS alUnits bRS bUnits outRS outUnits 0 0 0 0 where
        numFetch  = 2
        maxPhyReg = 31
        mem       = Mem.zeroed 255
        regs      = Mem.fromList (defaultedMem (replicate (maxPhyReg+1) Nothing) rrt)
        instrs'   = Mem.fromList instrs
        bypass    = BP.empty
        rob       = ROB.empty 4
        rrt       = RRT.fromRegs [pc, sp, lr, bp, ret] maxPhyReg
        memRS     = RS.empty
        memUnits  = [Unit.empty, Unit.empty, Unit.empty, Unit.empty]
        alRS      = RS.empty
        alUnits   = [Unit.empty, Unit.empty, Unit.empty, Unit.empty]
        bRS       = RS.empty
        bUnits    = [Unit.empty]
        outRS     = RS.empty
        outUnits  = [Unit.empty]

-- Create default Res with 32 ints of memory, and 16 registers.
emptyDefault :: [FInstr] -> State
emptyDefault = State.empty 11 12 13 14 15

-- Adds PC address at time of crash.
crash :: (InstrAddr -> Error) -> State -> Res a
crash f st = do
    pc <- pcVal st
    Crash (f pc) st

withBypass :: Bypass -> State -> State
withBypass b st = st { bypass = b }

-- Increments the number of cycles performed.
incCycles :: State -> State
incCycles st = st { cycles = (cycles st) + 1 }

-- Increments the number of instrucions exectuted.
incExec :: Int -> State -> State
incExec n st = st { instrsExec = (instrsExec st) + n }

-- Increments the number of flushes performed.
incFlushes :: State -> State
incFlushes st = st { flushes = (flushes st) + 1}

-- Increments the recorded number of cycles that were stalled.
incCyclesStalled :: State -> State
incCyclesStalled st = st { cyclesStalled = (cyclesStalled st) + 1 }

-- Return value of a register with matching index. Crash if invalid index.
findRegVal :: Q.Search -> PhyReg -> State -> Res (Maybe Val)
findRegVal robSearch i st =
    case BP.regVal i (bypass st) of
        Just val -> return (Just val)
        Nothing ->
            case ROB.regVal robSearch i (rob st) of
                Just val -> return (Just val)
                Nothing ->
                    case Reg.load i (regs st) of
                        Nothing  -> crash (RegOutOfRange i) st
                        Just val -> return val

-- Returns value of an address from bypass or memory. Crash if invalid address.
findMemVal :: Q.Search -> Addr -> State -> Res Val
findMemVal robSearch i st =
    case BP.memVal i (bypass st) of
        Just val -> return val
        Nothing ->
            case ROB.memVal robSearch i (rob st) of
                Just val -> return val
                Nothing ->
                    case Mem.load i (mem st) of
                        Nothing  -> crash (MemOutOfRange i) st
                        Just val -> return val

-- Return the value a register was most recently updated to.
newestRegVal :: PhyReg -> State -> Res (Maybe Val)
newestRegVal = findRegVal Q.NewToOld

-- Returns the index of the physical register mapped to the named register.
namedReg :: (State -> RegIdx) -> State -> Res PhyReg
namedReg getReg st = do
    let reg = getReg st
    getPhyReg reg st

-- Return the most recent value of a named register, e.g. PC
namedRegVal :: (State -> RegIdx) -> State -> Res Val
namedRegVal getReg st = do
    phy <- namedReg getReg st
    val <- newestRegVal phy st
    case val of
        Nothing  -> return 0
        Just val -> return val

-- Returns the value stored in the PC register.
pcVal :: State -> Res Val
pcVal = namedRegVal pcIdx

-- Set the PC to a given value.
setPC :: Val -> State -> Res State
setPC newPC st = do
    pcRegIdx <- namedReg pcIdx st
    pcReg <- namedReg pcIdx st
    st1 <- setRegVal pcReg (Just $ newPC) st
    let st2 = st1 { bypass=(bypass st1) ++ [BP.BypassReg pcRegIdx newPC] }
    return st2

-- Increment PC by given amount.
incPC :: Val -> State -> Res State
incPC n st = do
    pc <- pcVal st
    setPC (pc+n) st

-- Sets the value of a register in the physical register file.
setRegVal :: PhyReg -> Maybe Val -> State -> Res State
setRegVal i val st =
    case Reg.store i val (regs st) of
        Nothing   -> crash (RegOutOfRange i) st
        Just regs -> return st { regs = regs }

-- Used to clear a register (make non-ready) after its mapping from an
-- architectural register has been removed.
freeReg :: FreedReg -> State -> Res State
freeReg mPhy st1 =
    case mPhy of
        Nothing  -> return st1
        Just phy -> do
            st2 <- setRegVal phy Nothing st1
            return (st2 { rrt=RRT.free phy (rrt st2) })

-- Set the value at a memory address, or Crash if invalid address.
setMemVal :: Addr -> Val -> State -> Res State
setMemVal i val st =
    case Mem.store i val (mem st) of
        Nothing  -> crash (MemOutOfRange i) st
        Just mem -> return st { mem = mem }

-- Allocates a space in the ROB, and returns index of allocated space.
-- Returns Nothing if ROB is full.
-- TODO: Check whether ROB is full.
allocROB :: State -> (ROBIdx, State)
allocROB st = (idx, st { rob = rob' }) where
    (idx, rob') = ROB.alloc (rob st)

-- Places writeback instructions in the reorder buffer.
addROB :: State -> [PipeData WriteBack] -> State
addROB st wbs =
    let rob' = foldl (\rob (wb, idx, freed, savedPC) -> ROB.set idx wb freed savedPC rob) (rob st) wbs
    in st { rob = rob' }

-- Takes instruction that can be executed from ROB, to be passed to
-- write back stage.
commitROB :: State -> ([(WriteBack, FreedReg, RegMap)], Maybe SavedPC, State)
commitROB st =
    let (out, savedPC, rob') = ROB.commitable (rob st)
    in (out, savedPC, st { rob = rob' })

-- Allocates a mapping from an architectural to a physical register and stores
-- it in the ROB. Only if the instruction graduates from the ROB then the mapping
-- is stored in the RRT.
-- By initially storing mappings in the ROB, then they can be easily reverted
-- if a flush occurs.
allocPendingReg :: ROBIdx -> RegIdx -> State -> Res (PhyReg, State)
allocPendingReg robIdx reg st =
    case RRT.assign (rrt st) of
        Nothing -> crash NoFreePhyRegs st
        Just (phy, rrt') -> return (phy, st') where
            st' = st { rrt=rrt', rob = ROB.setRegMap robIdx reg phy (rob st) }

-- Sets the mapping from an architectural register to a physical register in the RRT.
-- The mapping was previously stored in the ROB.
confirmRegMap :: RegMap -> State -> State
confirmRegMap regMap st = st { rrt=rrt' } where
    rrt' = RRT.insMapping regMap (rrt st)

-- Takes a free physical register and returns its index.
-- Also returns the physical register that was freed, if the architectural
-- register was already mapped to a value.
allocPhyReg :: RegIdx -> State -> Res ((PhyReg, FreedReg), State)
allocPhyReg reg st =
    case RRT.ins reg (rrt st) of
        Nothing -> crash NoFreePhyRegs st
        Just (phy, rrt', freed) -> return ((phy, freed), st { rrt=rrt' })

getMaybePhyReg :: RegIdx -> State -> Res (Maybe PhyReg)
getMaybePhyReg reg st =
    case ROB.regMap reg (rob st) of
        Just phy -> return (Just phy)
        Nothing  ->
            case RRT.get reg (rrt st) of
                Nothing  -> return Nothing
                Just phy -> return (Just phy)

-- Returns physical register mapped to register name, or crashes if there
-- is no mapping.
getPhyReg :: RegIdx -> State -> Res PhyReg
getPhyReg reg st =
    case ROB.regMap reg (rob st) of
        Just phy -> return phy
        Nothing  ->
            case RRT.get reg (rrt st) of
                Nothing  -> crash (NoPhyRegAssigned reg) st
                Just phy -> return phy

-- Adds an instruction to its corresponding reservation station, e.g. branch
-- instruction goes to branch RS.
addRS :: DPipeInstr -> State -> State
addRS (Mem    di, idx, freed, savedPC) st = st { memRS = RS.add (RS.rsMemInstr di, idx, freed, savedPC) (memRS st)}
addRS (AL     di, idx, freed, savedPC) st = st { alRS  = RS.add (RS.rsALInstr  di, idx, freed, savedPC) (alRS  st)}
addRS (Branch di, idx, freed, savedPC) st = st { bRS   = RS.add (RS.rsBInstr   di, idx, freed, savedPC) (bRS   st)}
addRS (Out    di, idx, freed, savedPC) st = st { outRS = RS.add (RS.rsOutInstr di, idx, freed, savedPC) (outRS st)}

-- Returns instructions which have had all operands filled in and are ready
-- to execute.
runRS :: State
      -> (EMemInstr -> Res WriteBack)
      -> (EALInstr -> Res WriteBack)
      -> (State -> EBranchInstr -> Res WriteBack)
      -> (EOutInstr -> Res WriteBack)
      -> Res ([PipeData WriteBack], State)

runRS st1 lsu alu bu ou = do
    let rv robIdx phy  = findRegVal (Q.SubNewToOld robIdx) phy st1
        mv robIdx addr = findMemVal (Q.SubNewToOld robIdx) addr st1

    -- Try and fill in any available operands to instructions waiting in RS.
    memRS1 <- RS.fillMemRS rv mv (memRS st1)
    alRS1  <- RS.fillALRS  rv    (alRS  st1)
    bRS1   <- RS.fillBRS   rv    (bRS   st1)
    outRS1 <- RS.fillOutRS rv    (outRS st1)

    -- Take out any instructions which are ready and for which there is an
    -- execution unit available.
    (memUs1, memRS2) <- match memRS1 RS.promoteMemRS lsu (memUnits st1)
    (alUs1,  alRS2)  <- match alRS1  RS.promoteALRS  alu (alUnits  st1)
    (bUs1,   bRS2)   <- match bRS1   RS.promoteBRS   (bu st1)  (bUnits   st1)
    (outUs1, outRS2) <- match outRS1 RS.promoteOutRS ou  (outUnits st1)

    -- 'Run' instructions in exec units. Here, they actual wait some amount of
    -- time until the value in finally calculated by the caller of this function.
    let (memExecs1, memUs2) = runExecUnits memUs1
        (alExecs1,  alUs2)  = runExecUnits alUs1
        (bExecs1,   bUs2)   = runExecUnits bUs1
        (outExecs1, outUs2) = runExecUnits outUs1

        st2 = st1 {
            memRS=memRS2,    alRS=alRS2,    bRS=bRS2,    outRS=outRS2
          , memUnits=memUs2, alUnits=alUs2, bUnits=bUs2, outUnits=outUs2
        }

    return (memExecs1 ++ alExecs1 ++ bExecs1 ++ outExecs1, st2)

-- Gives instructions in RS that have all operands filled to available exec units.
match :: RS a -> (RS a -> (Maybe (PipeData b), RS a)) -> (b -> Res WriteBack) -> [ExecUnit (PipeData WriteBack)] -> Res ([ExecUnit (PipeData WriteBack)], RS a)
match rs promote execUnit units = foldM f ([], rs) units where
    f (accUnits, accRS) unit =
        case Unit.isFree unit of
            False -> return (unit:accUnits, accRS)
            -- Start an execution unit containing any ready to run instructions.
            True -> do
                let (i, accRS') = promote accRS
                case i of
                    Nothing -> return (Unit.empty:accUnits, accRS')
                    Just ei -> do
                        wb <- mapPipeDataM execUnit ei
                        return ((Unit.containing wb):accUnits, accRS')

-- Retrieve ready instructions from execution unit.
runExecUnits :: [ExecUnit b] -> ([b], [ExecUnit b])
runExecUnits = foldl f ([], []) where
    f (accExec, accUnits) unit =
        case Unit.value unit of
            Nothing -> (accExec, unit:accUnits)
            Just ei -> (ei:accExec, Unit.empty:accUnits)

-- Returns state which contains bypass value that was just written as part of
-- the write-back stage of the pipeline. This makes this value available to
-- previous stages of the pipeline.
bypassed :: [WriteBack] -> State -> State
bypassed wbs st = withBypass b st where
    b = BP.fromWbs wbs

-- Invalidates loads in ROB with same address.
invalidateLoads :: Addr -> State -> State
invalidateLoads addr st = st { rob=rob', memUnits=memUnits' } where
    rob'      = ROB.invalidateLoads addr (rob st)
    memUnits' = map (fmap (mapPipeData (invalidateLoad addr))) (memUnits st)

-- State after flusing the pipeline.
flush :: SavedPC -> [PhyReg] -> State -> Res State
flush savedPC frees st = setPC savedPC $ st {
    bypass = BP.empty
  , rob = ROB.flush (rob st)
  , rrt = RRT.freeAll frees (rrt st)

  , memRS = RS.empty
  , alRS = RS.empty
  , bRS = RS.empty
  , outRS = RS.empty

  , memUnits = map (const Unit.empty) (memUnits st)
  , alUnits  = map (const Unit.empty) (alUnits  st)
  , bUnits   = map (const Unit.empty) (bUnits   st)
  , outUnits = map (const Unit.empty) (outUnits st)
}
