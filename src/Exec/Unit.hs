module Exec.Unit
( Occupied
, ExecUnit(..)
, unit
, writeReg
, module Instr
, module RS
, module Pipeline
, module State
, module VM) where

import Instr
import RS hiding (empty)
import Pipeline hiding (empty)
import State hiding (empty)
import VM

type Occupied = Bool

-- Used to represent ALU, Load/Store Unit, etc
data ExecUnit = ExecUnit {
    run      :: Instr -> FilledOp -> State -> VM WriteBackInstr
  , occupied :: Occupied
}

-- Create execution unit that is unoccupied.
unit :: (Instr -> FilledOp -> State -> VM WriteBackInstr) -> ExecUnit
unit f = ExecUnit f False

writeReg :: RegIdx -> Val -> VM WriteBackInstr
writeReg reg val = return (WriteReg reg val)
