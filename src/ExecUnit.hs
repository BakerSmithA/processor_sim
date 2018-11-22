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
arithLogicUnit = unit al where
    al (MoveI r _) (UniOp x)   _ = WriteReg r x
    al (Move r _)  (UniOp x)   _ = WriteReg r x
    al (Add r _ _) (BinOp x y) _ = WriteReg r (x + y)

loadStoreUnit :: ExecUnit WriteBackInstr
loadStoreUnit = unit ls where
    ls = undefined

branchUnit :: ExecUnit WriteBackInstr
branchUnit = unit b where
    b = undefined
