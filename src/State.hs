module State where

import Mem (Mem)
import qualified Mem as Mem
import Instr
import Pipeline
import Bypass (Bypass)
import qualified Bypass as BP

-- Stores current state of virtual machine.
-- Uses Von Newmann architecture, and so data and instructions are separate.
data State = State {
    -- Memory
    mem    :: Mem Addr Val
  , regs   :: Mem RegIdx Val
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

   -- Stats
  , cycles :: Int
} deriving (Eq)

instance Show State where
    show st =
          "Cycles : " ++ show (cycles st)
     ++ "\nReg    : " ++ show (regs st)
     ++ "\nMem    : " ++ show (mem st)

-- Create state containing no values in memory or registers.
empty :: RegIdx -> RegIdx -> RegIdx -> RegIdx -> RegIdx -> [Instr] -> State
empty pc sp lr bp ret instrs = State mem regs instrs' pc sp lr bp ret [] BP.empty 0 where
    mem     = Mem.zeroed 256
    regs    = Mem.zeroed maxReg
    maxReg  = maximum [pc, sp, lr, bp, ret]
    instrs' = Mem.fromList instrs

-- Create default VM with 32 ints of memory, and 16 registers.
emptyDefault :: [Instr] -> State
emptyDefault = State.empty 12 13 14 15 16

withBypass :: Bypass -> State -> State
withBypass b st = st { bypass = b }

-- Increments the number of cycles performed.
incCycles :: State -> State
incCycles st = st { cycles = (cycles st) + 1 }
