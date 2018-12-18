module ExecUnit where

import Control.Monad (foldM)
import Data.Char (chr)
import State as St
import Instr
import WriteBack
import Types

exec :: [DPipeInstr] -> State ->  Res ([(PipeData WriteBack)], State)
exec dis st1 = do
    (eis, st2) <- issue dis st1
    execEs eis st2

-- Add all decoded instruction to a reservation station, then promote any completed,
-- instructions, and finally execute those and return the writeback instructions.
execEs :: [EPipeInstr] -> State -> Res ([(PipeData WriteBack)], State)
execEs eis st = do
    wbs <- foldM f [] eis
    return (wbs, St.bypassed (fmap pipeInstr wbs) st)
        where
            f allWbs ei = do
                wbs <- execI ei st
                return (wbs:allWbs)

-- Add decoded instructions to RS, and remove instructions from RS which have
-- all operands filled.
issue :: [DPipeInstr] -> State -> Res ([EPipeInstr], State)
issue dis st1 = do
    let st2 = foldr St.addRS st1 dis
    St.runRS st2

-- Add decoded instruction to a reservation station, then promote any completed,
-- instructions, and finally execute those and return the writeback instructions.
execI :: EPipeInstr -> State -> Res (PipeData WriteBack)
execI ei st = mapPipeDataM f ei where
    f (Mem    i) = lsu   i
    f (AL     i) = alu   i
    f (Branch i) = bu st i
    f (Out    i) = ou    i

-- Load/Store Unit.
lsu :: EMemInstr -> Res WriteBack
lsu (ELoad r val addr) = writeLoad r val addr
lsu (EStore val addr)  = writeMem addr val

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
bu :: State -> EBranchInstr -> Res WriteBack
bu st (B    addr) = branch addr st
bu st (BT r addr) = branchCond (r==1) addr st
bu st (BF r addr) = branchCond (r==0) addr st
bu _  (SysCall)   = return Terminate
bu st (Ret addr)  = branch (fromIntegral addr) st

-- Output Unit (for debugging).
ou :: EOutInstr -> Res WriteBack
ou (Print r)  = writePrint (show r)
ou (PrintC r) = writePrint ([chr (fromIntegral r)])
ou (PrintLn)  = writePrint "\n"

writeReg :: PhyReg -> Val -> Res WriteBack
writeReg r v = return (WriteReg r v (None False))

writeBranch :: PhyReg -> Val -> Res WriteBack
writeBranch r v = return (WriteReg r v (None True))

writeLoad :: PhyReg -> Val -> Addr -> Res WriteBack
writeLoad r v addr = return (WriteReg r v (ValidLoad addr))

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
    writeBranch pcReg addr'

-- Executes a branch if the condition is true, otherwise NoOp.
branchCond :: Bool -> Addr -> State -> Res WriteBack
branchCond True  addr st = branch addr st
branchCond False _    _  = return NoOp
