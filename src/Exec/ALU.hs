module Exec.ALU where

import Exec.Unit

arithLogicUnit :: ExecUnit
arithLogicUnit = unit al where
    al (Add  r _ _) (BinOp x y) _ = writeReg r (x + y)
    al (AddI r _ i) (UniOp x)   _ = writeReg r (x + i)
    al (Sub  r _ _) (BinOp x y) _ = writeReg r (x - y)
    al (SubI r _ i) (UniOp x)   _ = writeReg r (x - i)
    al (Mult r _ _) (BinOp x y) _ = writeReg r (x * y)
    al (Div  r _ _) (BinOp x y) _ = writeReg r (x `div` y)

    al (Eq  r _ _) (BinOp x y) _ = writeReg r (eqVal x y)
    al (Lt  r _ _) (BinOp x y) _ = writeReg r (ltVal x y)
    al (Or  r _ _) (BinOp x y) _ = writeReg r (orVal x y)
    al (And r _ _) (BinOp x y) _ = writeReg r (andVal x y)
    al (Not r _)   (UniOp x)   _ = writeReg r (notVal x)

    al _ _ _ = error "unexpected al"

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
