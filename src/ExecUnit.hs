module ExecUnit where

import State
import Instr
import WriteBack

-- Load/Store Unit.
lsu :: EInstr -> State -> Res WriteBack
lsu (LoadIdx      r b off) st = load  r b off st
lsu (LoadBaseIdx  r b off) st = load  r b off st
lsu (StoreIdx     r b off) _  = store r b off
lsu (StoreBaseIdx r b off) _  = store r b off
lsu _                      _  = error "Unsupported LSU Instr"

-- Arithmetic/Logic Unit.
alu :: EInstr -> Res WriteBack
alu (MoveI r i)   = writeReg r i
alu (Move  r x)   = writeReg r x
alu (Add   r x y) = writeReg r (x + y)
alu (AddI  r x i) = writeReg r (x + i)
alu (Sub   r x y) = writeReg r (x - y)
alu (SubI  r x i) = writeReg r (x - i)
alu (Mult  r x y) = writeReg r (x * y)
alu (Div   r x y) = writeReg r (x `div` y)
alu (Eq    r x y) = writeReg r (x `eqVal` y)
alu (Or    r x y) = writeReg r (x `orVal` y)
alu (And   r x y) = writeReg r (x `andVal` y)
alu (Not   r x)   = writeReg r (notVal x)
alu _             = error "Unsupported ALU Instr"

-- Branch Unit.
bu :: EInstr -> WriteBack
bu = undefined

-- Output Unit (for debugging).
ou :: EInstr -> WriteBack
ou = undefined

-- Convenience method for WriteReg in Res.
writeReg :: PhyReg -> Val -> Res WriteBack
writeReg r v = return (WriteReg r v)

-- Loads contents of memory at address into register.
load :: PhyReg -> Val -> Val -> State -> Res WriteBack
load r base off st = do
    val <- memVal (fromIntegral $ base + off) st
    return (WriteReg r val)

-- Stores contents of register into memory address.
store :: Val -> Val -> Val -> Res WriteBack
store val base off =
    return (WriteMem (fromIntegral $ base+off) val)

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
