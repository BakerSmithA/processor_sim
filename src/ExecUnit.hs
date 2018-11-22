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

loadStoreUnit :: ExecUnit WriteBackInstr
loadStoreUnit = unit ls where
    ls (MoveI r i)          (EmptyOp)     _ = WriteReg r i
    ls (Move r _)           (UniOp x)     _ = WriteReg r x
    ls (LoadIdx r _ off)    (UniOp base)  _ = undefined
    ls (LoadBaseIdx r _ _)  (BinOp b off) _ = undefined
    ls (StoreIdx r _ off)   (UniOp base)  _ = undefined
    ls (StoreBaseIdx r _ _) (BinOp b off) _ = undefined

arithLogicUnit :: ExecUnit WriteBackInstr
arithLogicUnit = unit al where
    al (Add r _ _)  (BinOp x y) _ = WriteReg r (x + y)
    al (AddI r _ i) (UniOp x)   _ = WriteReg r (x + i)

branchUnit :: ExecUnit WriteBackInstr
branchUnit = unit b where
    b = undefined
