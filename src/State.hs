module State where

import Mem (Mem)
import qualified Mem as Mem
import qualified Mem as Reg
import Instr
import Bypass (Bypass)
import qualified Bypass as BP
import ROB (ROB, ROBIdx)
import qualified ROB as ROB
import Error
import WriteBack
import RRT

-- Stores current state of CPU at a point in time.
-- Uses Von Newmann architecture, and so data and instructions are separate.
data State = State {
    -- Memory
    mem    :: Mem Addr Val
    -- Physical register file. Distinct from names of registers given in ASM.
  , regs   :: Mem PhyReg Val
  , instrs :: Mem Addr Instr

    -- Register indices
  , pcIdx  :: RegIdx -- Program Counter
  , spIdx  :: RegIdx -- Stack Pointer
  , lrIdx  :: RegIdx -- Link Register
  , bpIdx  :: RegIdx -- Base Pointer
  , retIdx :: RegIdx -- Return value register (EAX in x86)

    -- Output
  , output :: String

   -- Pipeline
  , bypass :: Bypass
  , rob    :: ROB
  , rrt    :: RRT

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

-- Create state containing no values in memory or registers.
empty :: RegIdx -> RegIdx -> RegIdx -> RegIdx -> RegIdx -> [Instr] -> State
empty pc sp lr bp ret instrs = State mem regs instrs' pc sp lr bp ret [] bypass rob rrt 0 0 where
    maxPhyReg = 36
    mem       = Mem.zeroed 127
    regs      = Mem.zeroed maxPhyReg
    instrs'   = Mem.fromList instrs
    bypass    = BP.empty
    rob       = ROB.empty 15
    rrt       = RRT.fromRegs pc sp lr bp ret maxPhyReg

-- Create default Res with 32 ints of memory, and 16 registers.
emptyDefault :: [Instr] -> State
emptyDefault = State.empty 11 12 13 14 15

withBypass :: Bypass -> State -> State
withBypass b st = st { bypass = b }

-- Increments the number of cycles performed.
incCycles :: State -> State
incCycles st = st { cycles = (cycles st) + 1 }

-- Increments the number of instrucions exectuted.
incExec :: State -> State
incExec st = st { instrsExec = (instrsExec st) + 1 }

-- Return value of a register, from bypass or register. Crash if invalid index.
regVal :: RegIdx -> State -> Res Val
regVal i st =
    case BP.regVal i (bypass st) of
        Just val -> return val
        Nothing ->
            case ROB.regVal i (rob st) of
                Just val -> return val
                Nothing ->
                    case Reg.load i (regs st) of
                        Nothing  -> crash (RegOutOfRange i) st
                        Just val -> return val

-- Returns value of an address from bypass or memory. Crash if invalid address.
memVal :: Addr -> State -> Res Val
memVal i st =
    case BP.memVal i (bypass st) of
        Just val -> return val
        Nothing ->
            case ROB.memVal i (rob st) of
                Just val -> return val
                Nothing ->
                    case Mem.load i (mem st) of
                        Nothing  -> crash (MemOutOfRange i) st
                        Just val -> return val

-- Allocates a space in the ROB, and returns index of allocated space.
-- Returns Nothing if ROB is full.
-- TODO: Check whether ROB is full.
allocROB :: State -> Maybe (ROBIdx, State)
allocROB st = Just (idx, st { rob = rob' }) where
    (idx, rob') = ROB.alloc (rob st)

commit :: State -> [(ROBIdx, WriteBack)] -> ([WriteBack], State)
commit st wbs =
    let rob' = foldl (flip (uncurry ROB.set)) (rob st) wbs
        (out, rob'') = ROB.commitable rob'
    in (out, st { rob = rob'' })

-- Adds PC address at time of crash.
crash :: (InstrAddr -> Error) -> State -> Res a
crash f st = do
    pc <- fmap fromIntegral (regVal (pcIdx st) st)
    Crash (f pc) st
