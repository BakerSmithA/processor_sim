module Exec.Unit
( Occupied
, ExecUnit(..)
, unit
, writeReg
, crash
, module Instr
, module RS
, module Pipeline
, module State
, module Result) where

import Instr
import RS hiding (empty)
import Pipeline hiding (empty)
import State hiding (empty)
import Result
import Mem as Reg
import Data.Maybe (fromJust)

type Occupied = Bool

-- Used to represent ALU, Load/Store Unit, etc
data ExecUnit = ExecUnit {
    run      :: Instr -> FilledOp -> State -> Result WriteBackInstr
  , occupied :: Occupied
}

-- Create execution unit that is unoccupied.
unit :: (Instr -> FilledOp -> State -> Result WriteBackInstr) -> ExecUnit
unit f = ExecUnit f False

writeReg :: RegIdx -> Val -> Result WriteBackInstr
writeReg reg val = return (WriteReg reg val)

-- Crash VM with error where PC is supplied by getting the current PC.
crash :: (InstrAddr -> Error) -> State -> Result a
crash f st = Crash (f (pc st)) st
