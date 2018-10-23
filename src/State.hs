module State where

import Mem (Mem)
import qualified Mem as Mem
import Instr
import Pipeline

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
} deriving (Eq)

instance Show State where
    show st =
        "Mem: "   ++ show (mem st)
     ++ "\nReg: " ++ show (regs st)
