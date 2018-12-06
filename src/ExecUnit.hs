module ExecUnit where

import Data.Char (chr)
import Control.Monad (foldM)
import State as St
import Instr
import WriteBack
import Types

-- Add decoded instruction to a reservation station, then promote any completed,
-- instructions, and finally execute those and return the writeback instructions.
exec :: DInstrIdx -> State -> Res ([(WriteBack, ROBIdx)], State)
exec di st1 = do
    let st2 = St.addRS di st1
    (execIs, st3) <- St.runRS st2
    wbs <- foldM (combine st3) [] execIs
    return (wbs, st3)

-- Accumulate the writebacks of multiple executed instructions.
combine :: State -> [(WriteBack, ROBIdx)] -> EInstrIdx -> Res [(WriteBack, ROBIdx)]
combine st acc instr = do
    wb <- execWithROBIdx instr st
    return (wb:acc)

-- Executes the instruction and passes the associate reorder buffer index through.
execWithROBIdx :: EInstrIdx -> State -> Res (WriteBack, ROBIdx)
execWithROBIdx (instr, idx) st = do
    wb <- execI instr st
    return (wb, idx)

-- Execute an instruction by sending it to the appropriate unit, e.g. ALU.
execI :: EInstr -> State -> Res WriteBack
execI instr st | isMem    instr = lsu instr st
               | isAL     instr = alu instr
               | isBranch instr = bu  instr st
               | isOut    instr = ou  instr
               | otherwise      = errUnsupported "" instr

-- Load/Store Unit.
lsu :: EInstr -> State -> Res WriteBack
lsu (LoadIdx      r b off) st = load  r b off st
lsu (LoadBaseIdx  r b off) st = load  r b off st
lsu (StoreIdx     r b off) _  = store r b off
lsu (StoreBaseIdx r b off) _  = store r b off
lsu i                      _  = errUnsupported "LSU" i

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
alu (Lt    r x y) = writeReg r (x `ltVal` y)
alu (Or    r x y) = writeReg r (x `orVal` y)
alu (And   r x y) = writeReg r (x `andVal` y)
alu (Not   r x)   = writeReg r (notVal x)
alu i             = errUnsupported "ALU" i

-- Branch Unit.
bu :: EInstr -> State -> Res WriteBack
bu (B addr)    st = branch addr st
bu (BT r addr) st = branchCond (r==1) addr st
bu (BF r addr) st = branchCond (r==0) addr st
bu (SysCall)   _  = return Terminate
bu (Ret)       st  = do
    lrReg <- namedReg lrIdx st
    addr <- regVal lrReg st
    branch (fromIntegral addr) st
bu  i         _  = errUnsupported "BU" i

-- Output Unit (for debugging).
ou :: EInstr -> Res WriteBack
ou (Print r)  = writePrint (show r)
ou (PrintC r) = writePrint ([chr (fromIntegral r)])
ou (PrintLn)  = writePrint "\n"
ou i          = errUnsupported "OU" i

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

writePrint :: String -> Res WriteBack
writePrint = return . WritePrint

errUnsupported :: String -> EInstr -> a
errUnsupported s i = error ("Unsupported " ++ s ++ "Instr: " ++ show i)
