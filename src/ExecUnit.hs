module ExecUnit where

import Instr
import RS
import Pipeline
import State

type Occupied = Bool

-- Used to represent ALU, Load/Store Unit, etc
data ExecUnit a = ExecUnit (Instr -> FilledOp -> State -> a) Occupied

-- Create execution unit that is unoccupied.
unit :: (Instr -> FilledOp -> State -> a) -> ExecUnit a
unit f = ExecUnit f False

arithLogicUnit :: ExecUnit WriteBackInstr
arithLogicUnit = undefined

loadStoreUnit :: ExecUnit WriteBackInstr
loadStoreUnit = undefined

branchUnit :: ExecUnit WriteBackInstr
branchUnit = undefined
