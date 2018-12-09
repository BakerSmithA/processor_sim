module ExecUnit where

import Data.Char (chr)
import Control.Monad (foldM)
import State as St
import Instr
import WriteBack
import Types

-- Add decoded instruction to a reservation station, then promote any completed,
-- instructions, and finally execute those and return the writeback instructions.
exec :: DPipeInstr -> State -> Res ([(PipeData WriteBack)], State)
exec di st1 = do
    let st2 = St.addRS di st1
    (mems, als, bs, outs, st2) <- St.runRS st2
    undefined



-- Load/Store Unit.
lsu :: EMemInstr -> Res WriteBack
lsu (ELoad r val _)   = writeReg r val
lsu (EStore val addr) = writeMem addr val

-- Arithmetic/Logic Unit.
alu :: EALInstr -> Res WriteBack
alu (MoveI r i)   = writeReg r i
alu (Move  r x)   = writeReg r x
alu (Add   r x y) = writeReg r (x + y)
alu (AddI  r x i) = writeReg r (x + i)
alu (Sub   r x y) = writeReg r (x - y)
alu (SubI  r x i) = writeReg r (x - i)
alu (Mult  r x y) = writeReg r (x * y)
alu (Div   r x y) = writeReg r (x `div` y)
alu (Eq    r x y) = writeReg r (x `eqVal` y)
alu (Lt    r x y) = writeReg r (x `ltVal` y)
alu (Or    r x y) = writeReg r (x `orVal` y)
alu (And   r x y) = writeReg r (x `andVal` y)
alu (Not   r x)   = writeReg r (notVal x)

-- Branch Unit.
bu :: EBranchInstr -> State -> Res WriteBack
bu (B    addr) st = branch addr st
bu (BT r addr) st = branchCond (r==1) addr st
bu (BF r addr) st = branchCond (r==0) addr st
bu (SysCall)   _  = return Terminate
bu (Ret addr)  st = branch (fromIntegral addr) st

-- Output Unit (for debugging).
ou :: EOutInstr -> Res WriteBack
ou (Print r)  = writePrint (show r)
ou (PrintC r) = writePrint ([chr (fromIntegral r)])
ou (PrintLn)  = writePrint "\n"

-- Convenience method for WriteReg in Res.
writeReg :: PhyReg -> Val -> Res WriteBack
writeReg r v = return (WriteReg r v)

writeMem :: Addr -> Val -> Res WriteBack
writeMem addr v = return (WriteMem (fromIntegral addr) v)

writePrint :: String -> Res WriteBack
writePrint = return . WritePrint

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

-- Executes a branch by writing PC.
branch :: Addr -> State -> Res WriteBack
branch addr st = do
    pcReg <- namedReg pcIdx st
    -- +1 because pipeline stalls until branch executed, and PC not updated.
    let addr' = fromIntegral (addr+1)
    return (WriteReg pcReg addr')

-- Executes a branch if the condition is true, otherwise NoOp.
branchCond :: Bool -> Addr -> State -> Res WriteBack
branchCond True  addr st = branch addr st
branchCond False _    _  = return NoOp
