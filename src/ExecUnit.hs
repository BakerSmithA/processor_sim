module ExecUnit where

import Instr
import RS
import Pipeline
import State

type Occupied = Bool

-- Used to represent ALU, Load/Store Unit, etc
data ExecUnit a = ExecUnit {
    run      :: Instr -> FilledOp -> State -> a
  , occupied :: Occupied
}

-- Create execution unit that is unoccupied.
unit :: (Instr -> FilledOp -> State -> a) -> ExecUnit a
unit f = ExecUnit f False

loadStoreUnit :: ExecUnit WriteBackInstr
loadStoreUnit = unit ls where
    ls (MoveI        r i)     (EmptyOp)     _ = WriteReg r i
    ls (Move         r _)     (UniOp x)     _ = WriteReg r x
    ls (LoadIdx      r _ off) (UniOp base)  _ = undefined
    ls (LoadBaseIdx  r _ _)   (BinOp b off) _ = undefined
    ls (StoreIdx     r _ off) (UniOp base)  _ = undefined
    ls (StoreBaseIdx r _ _)   (BinOp b off) _ = undefined

    ls _ _ _ = error "unexpected ls"

arithLogicUnit :: ExecUnit WriteBackInstr
arithLogicUnit = unit al where
    al (Add  r _ _) (BinOp x y) _ = WriteReg r (x + y)
    al (AddI r _ i) (UniOp x)   _ = WriteReg r (x + i)
    al (Sub  r _ _) (BinOp x y) _ = WriteReg r (x - y)
    al (SubI r _ i) (UniOp x)   _ = WriteReg r (x - i)
    al (Mult r _ _) (BinOp x y) _ = WriteReg r (x * y)
    al (Div  r _ _) (BinOp x y) _ = WriteReg r (x `div` y)

    al (Eq  r _ _) (BinOp x y) _ = WriteReg r (eqVal x y)
    al (Lt  r _ _) (BinOp x y) _ = WriteReg r (ltVal x y)
    al (Or  r _ _) (BinOp x y) _ = WriteReg r (orVal x y)
    al (And r _ _) (BinOp x y) _ = WriteReg r (andVal x y)
    al (Not r _)   (UniOp x)   _ = WriteReg r (notVal x)

    al _ _ _ = error "unexpected al"

branchUnit :: ExecUnit WriteBackInstr
branchUnit = unit b where
    b = undefined

eqVal :: Val -> Val -> Val
eqVal x y | x == y    = 1
          | otherwise = 0

ltVal :: Val -> Val -> Val
ltVal x y | x < y     = 1
          | otherwise = 0

orVal :: Val -> Val -> Val
orVal x y | x == 1 || y == 1 = 1
          | otherwise        = 0

andVal :: Val -> Val -> Val
andVal x y | x == 1 && y == 1 = 1
           | otherwise        = 0

notVal :: Val -> Val
notVal x | x == 1    = 0
         | otherwise = 1
