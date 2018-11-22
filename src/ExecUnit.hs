module ExecUnit where

import Instr
import RS
import Pipeline
import State

type Occupied = Bool

-- Used to represent ALU, Load/Store Unit, etc
data ExecUnit = ExecUnit {
    run      :: Instr -> FilledOp -> State -> WriteBackInstr
  , occupied :: Occupied
}

-- Create execution unit that is unoccupied.
unit :: (Instr -> FilledOp -> State -> WriteBackInstr) -> ExecUnit
unit f = ExecUnit f False

loadStoreUnit :: ExecUnit
loadStoreUnit = unit ls where
    ls (MoveI        r i)     (EmptyOp)     _ = WriteReg r i
    ls (Move         r _)     (UniOp x)     _ = WriteReg r x
    ls (LoadIdx      r _ off) (UniOp base)  _ = undefined
    ls (LoadBaseIdx  r _ _)   (BinOp b off) _ = undefined
    ls (StoreIdx     r _ off) (UniOp base)  _ = undefined
    ls (StoreBaseIdx r _ _)   (BinOp b off) _ = undefined

    ls _ _ _ = error "unexpected ls"

arithLogicUnit :: ExecUnit
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

branchUnit :: ExecUnit
branchUnit = unit b where
    b (B addr)    (EmptyOp) st = branch addr st
    b (BT _ addr) (UniOp x) st = branchCond (x==1) addr st
    b (BF _ addr) (UniOp x) st = branchCond (x/=1) addr st
    b (Ret)       (EmptyOp) st = undefined
    b (SysCall)   (EmptyOp) st = undefined

-- Executes a branch by writing PC.
branch :: Addr -> State -> WriteBackInstr
branch addr st = WriteReg pc addr' where
    pc = pcIdx st
    -- +1 because pipeline stalls until branch executed, and PC not updated.
    addr' = fromIntegral (addr+1)

-- Executes a branch if the value in a register passes a condition, otherwise NoOp.
branchCond :: Bool -> Addr -> State -> WriteBackInstr
branchCond cond addr st = do
    if cond
        then branch addr st
        else NoOp

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
