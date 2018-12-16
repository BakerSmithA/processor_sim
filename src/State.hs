module State where

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
import qualified RS as RS
import Types

-- Stores current state of CPU at a point in time.
-- Uses Von Newmann architecture, and so data and instructions are separate.
data State = State {
    -- Memory
    mem    :: Mem Addr Val
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
   -- Load/Store Queue
  , memRS  :: MemRS
  , alRS   :: ArithLogicRS
  , bRS    :: BranchRS
  , outRS  :: OutRS

   -- Stats
  , cycles :: Int
  , instrsExec :: Int

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
    -- (>>=) :: m a -> (a -> m b) -> m b
    (Res x)      >>= f = f x
    (Crash e st) >>= _ = Crash e st
    (Exit st)    >>= _ = Exit st

instance Show State where
    show st =
          "Cycles : "  ++ show (cycles st)
     ++ "\nInstrs : "  ++ show (instrsExec st)
     ++ "\nIpC    : "  ++ show ((fromIntegral $ instrsExec st) / (fromIntegral $ cycles st) :: Double)
     ++ "\nReg    : "  ++ Mem.showNumbered (regs st)
     ++ "\nMem    :\n" ++ Mem.showBlocks 16 (mem st)

debugShow :: State -> String
debugShow st =
        "\nBypass : "  ++ show (bypass st)
     ++ "\nRRT    : "  ++ show (rrt st)
     ++ "\nROB    : "  ++ show (rob st)
     ++ "\nMemRS    : "  ++ show (memRS st)
     ++ "\nAL  RS : "  ++ show (alRS st)
     ++ "\nB   RS : "  ++ show (bRS st)
     ++ "\nOut RS : "  ++ show (outRS st)
     ++ "\nReg    : "  ++ Mem.showNumbered (regs st)
     ++ "\nMem    :\n" ++ Mem.showBlocks 16 (mem st)

-- Create state containing no values in memory or registers.
empty :: RegIdx -> RegIdx -> RegIdx -> RegIdx -> RegIdx -> [FInstr] -> State
empty pc sp lr bp ret instrs = State mem regs instrs' pc sp lr bp ret [] bypass rob rrt memRS alRS bRS outRS 0 0 where
    maxPhyReg = 15
    mem       = Mem.zeroed 255
    regs      = Mem.fromList (replicate (maxPhyReg+1) (Just 0))
    instrs'   = Mem.fromList instrs
    bypass    = BP.empty
    rob       = ROB.empty 5
    rrt       = RRT.fromConstRegs [pc, sp, lr, bp, ret] maxPhyReg
    memRS       = RS.empty
    alRS      = RS.empty
    bRS       = RS.empty
    outRS     = RS.empty

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
incExec :: State -> State
incExec st = st { instrsExec = (instrsExec st) + 1 }

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
        Nothing  -> error "Named reg contained no value"
        Just val -> return val

-- Returns the value stored in the PC register.
pcVal :: State -> Res Val
pcVal = namedRegVal pcIdx

-- Sets the value of a register in the physical register file.
setRegVal :: PhyReg -> Maybe Val -> State -> Res State
setRegVal i val st =
    case Reg.store i val (regs st) of
        Nothing   -> crash (RegOutOfRange i) st
        Just regs -> return st { regs = regs }

-- Used to clear a register (make non-ready) after its mapping from an
-- architectural register has been removed.
clearFreedReg :: FreedReg -> State -> Res State
clearFreedReg mPhy st =
    case mPhy of
        Nothing  -> return st
        Just phy -> setRegVal phy Nothing st

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
addROB :: State -> [(WriteBack, ROBIdx, FreedReg)] -> State
addROB st wbs =
    let rob' = foldl (\rob (wb, idx, freed) -> ROB.set idx wb freed rob) (rob st) wbs
    in st { rob = rob' }

-- Takes instruction that can be executed from ROB, to be passed to
-- write back stage.
commitROB :: State -> ([(WriteBack, FreedReg)], State)
commitROB st =
    let (out, rob') = ROB.commitable (rob st)
    in (out, st { rob = rob' })

-- Takes a free physical register and returns its index.
-- Also returns the physical register that was freed, if the architectural
-- register was already mapped to a value.
allocPhyReg :: RegIdx -> State -> Res ((PhyReg, FreedReg), State)
allocPhyReg reg st =
    case RRT.ins reg (rrt st) of
        Nothing -> crash NoFreePhyRegs st
        Just (phy, rrt', freed) -> return ((phy, freed), st { rrt=rrt' })

-- Frees a physical register allocated to a register name.
-- Crashes if there if no mapping to the physical register.
freePhyReg :: PhyReg -> State -> State
freePhyReg phy st = st { rrt=RRT.free phy (rrt st) }

-- Returns physical register mapped to register name, or crashes if there
-- is no mapping.
getPhyReg :: RegIdx -> State -> Res PhyReg
getPhyReg reg st =
    case RRT.get reg (rrt st) of
        Nothing  -> crash (NoPhyRegAssigned reg) st
        Just phy -> return phy

-- Adds an instruction to its corresponding reservation station, e.g. branch
-- instruction goes to branch RS.
addRS :: DPipeInstr -> State -> State
addRS (Mem    di, idx, freed) st = st { memRS = RS.add (RS.rsMemInstr di, idx, freed) (memRS st)}
addRS (AL     di, idx, freed) st = st { alRS  = RS.add (RS.rsALInstr  di, idx, freed) (alRS  st)}
addRS (Branch di, idx, freed) st = st { bRS   = RS.add (RS.rsBInstr   di, idx, freed) (bRS   st)}
addRS (Out    di, idx, freed) st = st { outRS = RS.add (RS.rsOutInstr di, idx, freed) (outRS st)}

-- Returns instructions which have had all operands filled in and are ready
-- to execute.
runRS :: State -> Res ([PipeData EMemInstr], [PipeData EALInstr], [PipeData EBranchInstr], [PipeData EOutInstr], State)
runRS st = do
    let rv robIdx phy  = findRegVal (Q.SubNewToOld robIdx) phy st
        mv robIdx addr = findMemVal (Q.SubNewToOld robIdx) addr st

    (memExecs, memRS) <- RS.runMemRS rv mv (memRS st)
    (alExecs,  alRS)  <- RS.runAL    rv    (alRS  st)
    (bExecs,   bRS)   <- RS.runB     rv    (bRS   st)
    (outExecs, outRS) <- RS.runOut   rv    (outRS st)

    let st' = st { memRS=memRS, alRS=alRS, bRS=bRS, outRS=outRS}
    return (memExecs, alExecs, bExecs, outExecs, st')
