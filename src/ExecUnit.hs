module ExecUnit where

import Instr
import WriteBack

-- Load/Store Unit.
lsu :: EInstr -> WriteBack
lsu = undefined

-- Arithmetic/Logic Unit.
alu :: EInstr -> WriteBack
alu (MoveI r i)   = WriteReg r i
alu (Move  r x)   = WriteReg r x
alu (Add   r x y) = WriteReg r (x + y)
alu (AddI  r x i) = WriteReg r (x + i)
alu (Sub   r x y) = WriteReg r (x - y)
alu (SubI  r x i) = WriteReg r (x - i)
alu (Mult  r x y) = WriteReg r (x * y)
alu (Div   r x y) = WriteReg r (x `div` y)
alu (Eq    r x y) = WriteReg r (x `eqVal` y)
alu (Or    r x y) = WriteReg r (x `orVal` y)
alu (And   r x y) = WriteReg r (x `andVal` y)
alu (Not   r x)   = WriteReg r (notVal x)
alu _             = error "Unsupported ALU Instr"

-- Branch Unit.
bu :: EInstr -> WriteBack
bu = undefined

-- Output Unit (for debugging).
ou :: EInstr -> WriteBack
ou = undefined

-- Boolean logic for values.
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
