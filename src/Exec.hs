module Exec where

import Control.Monad (foldM)
import State as St
import Error
import Instr
import Pipeline as P
import qualified Mem as Mem
import qualified Mem as Reg
import qualified Bypass as BP
import ROB (ROBIdx)
import WriteBack
import RRT
import Debug.Trace

-- E.g. Mult, Add, And, Or, etc
type ValOp = (Val -> Val -> Val)

-- Loads contents of memory at address into register.
load :: Addr -> RegIdx -> State -> Res WriteBack
load addr r st = do
    val <- memVal addr st
    return (WriteReg r val)

-- Stores contents of register into memory address.
store :: RegIdx -> Addr -> State -> Res WriteBack
store r addr st = do
    val <- regVal r st
    return (WriteMem addr val)

-- Perform operation on value stored in register, and immediate value.
opI :: RegIdx -> RegIdx -> ValOp -> Val -> State -> Res WriteBack
opI r x f imm st = do
    val <- regVal x st
    return (WriteReg r (f val imm))

-- Perform operation on two values stored in registers.
opReg :: RegIdx -> RegIdx -> ValOp -> RegIdx -> State -> Res WriteBack
opReg r x f y st = do
    val <- regVal y st
    opI r x f val st

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

-- Perform fetch state of pipeline by retreiving instruction, this instruction
-- is then allocated a space in the ROB to have its result written to.
-- Or, return Nothing if the value of the pc is after the last instruction,
-- or there is no space in the ROB.
fetch :: State -> Res (Maybe (ROBIdx, Instr), State)
fetch st = trace ("FET:\n" ++ show st ++ "\n") $ do
    pc <- fmap fromIntegral (regVal (pcIdx st) st)
    case Mem.load pc (instrs st) >>= allocFetched st of
        Nothing                -> return (Nothing, st)
        Just (idx, instr, st') -> return (Just (idx, instr), st')

-- Allocate a space in the ROB for an instruction that was fetched.
-- Returns an index in the ROB to write the result of the instruction to, or
-- Nothing if the ROB is full.
allocFetched :: State -> Instr -> Maybe (ROBIdx, Instr, State)
allocFetched st instr = do
    (idx, st') <- St.allocROB st
    return (idx, instr, st')

-- Because instruction are already parsed into struct, no need to decode.
-- However, register renaming will be performed at this step.
decode :: Instr -> State -> Res (Instr, State)
decode i st = trace ("DEC:\n" ++ show st ++ "\n") $ return (i, st)

-- Executes a branch by writing PC.
branch :: Addr -> State -> Res WriteBack
branch addr st = return (WriteReg pc addr') where
    pc = pcIdx st
    -- +1 because pipeline stalls until branch executed, and PC not updated.
    addr' = fromIntegral (addr+1)

-- Executes a branch if the value in a register passes a condition, otherwise NoOp.
branchCond :: RegIdx -> (Val -> Bool) -> Addr -> State -> Res WriteBack
branchCond reg cond addr st = do
    val <- regVal reg st
    if cond val
        then branch addr st
        else return NoOp

-- Perform execution stage of pipeline, and generate instruction of what
-- to modify in machine.
exec :: Instr -> State -> Res WriteBack
-- Memory
-- Move immediate value into register.
exec (MoveI r val) _ =
    return (WriteReg r val)
-- Move value from one register into another.
exec (Move r from) st = do
    val <- regVal from st
    return (WriteReg r val)
-- Load value from memory into register, where reg and immediate provide
-- base and offset.
exec (LoadIdx r base offset) st = do
    baseAddr <- regVal base st
    load (fromIntegral (baseAddr + offset)) r st
-- Load value from memory into register, where two regs provide base and offset.
exec (LoadBaseIdx r base rOffset) st = do
    baseAddr <- regVal base st
    offsetAddr <- regVal rOffset st
    load (fromIntegral (baseAddr + offsetAddr)) r st
-- Store value from register into memory, where reg and immediate provide
-- base and offset.
exec (StoreIdx r base offset) st = do
    baseAddr <- regVal base st
    store r (fromIntegral (baseAddr + offset)) st
-- Store value from register into memory, where two regs provide base and offset.
exec (StoreBaseIdx r base rOffset) st = do
    baseAddr <- regVal base st
    offsetAddr <- regVal rOffset st
    store r (fromIntegral (baseAddr + offsetAddr)) st
-- Arithmetic/Logic
exec (Add r x y)  st = opReg r x (+) y st
exec (AddI r x i) st = opI r x (+) i st
exec (Sub r x y)  st = opReg r x (-) y st
exec (SubI r x i) st = opI r x (-) i st
exec (Mult r x y) st = opReg r x (*) y st
exec (Div r x y)  st = opReg r x (div) y st
exec (Eq r x y)   st = opReg r x eqVal y st
exec (Lt r x y)   st = opReg r x ltVal y st
exec (Or r x y)   st = opReg r x orVal y st
exec (And r x y)  st = opReg r x andVal y st
exec (Not r x)    st = do
    val <- regVal x st
    return (WriteReg r (notVal val))
-- Branching
-- Unconditional branch to address.
exec (B addr) st = branch addr st
-- Branch if value in register is true.
exec (BT r addr) st = branchCond r (==1) addr st
-- Branch if value in register is false.
exec (BF r addr) st = branchCond r (==0) addr st
-- Branch to value stored in link register.
exec (Ret) st = do
    addr <- regVal (lrIdx st) st
    branch (fromIntegral addr) st
-- Terminate execution of the program.
exec (SysCall) _ =
    return Terminate
-- Debugging.
-- Print value in register.
exec (Print r) st = do
    val <- regVal r st
    return (WritePrint (show val))
-- Print newline.
exec (PrintLn) _ =
    return (WritePrint "\n")

-- Places executed results in reorder buffer.
commit :: (ROBIdx, WriteBack) -> State -> Res ([WriteBack], State)
commit wb st = trace ("COM:\n" ++ show st ++ "\n") $ return (St.commit st [wb])

-- Set the value stored in a register, or Crash if invalid index.
setRegVal :: RegIdx -> Val -> State -> Res State
setRegVal i val st =
    case Reg.store i val (regs st) of
        Nothing   -> crash (RegOutOfRange i) st
        Just regs -> return st { regs = regs }

-- Set the value at a memory address, or Crash if invalid address.
setMemVal :: Addr -> Val -> State -> Res State
setMemVal i val st =
    case Mem.store i val (mem st) of
        Nothing  -> crash (MemOutOfRange i) st
        Just mem -> return st { mem = mem }

-- Add string to output of Res.
addOutput :: String -> State -> Res State
addOutput s st = return st { output = (output st) ++ s }

-- Perform write-back stage of pipeline, writing result back to register/memory.
writeBack :: [WriteBack] -> State -> Res State
writeBack is st = trace ("WB:\n" ++ show st ++ "\n") $ do
    st' <- foldM writeBack' st is
    return (St.incExec st')
        where
            writeBack' st (WriteReg r val) = setRegVal r val st
            writeBack' st (WriteMem i val) = setMemVal i val st
            writeBack' st (WritePrint s)   = addOutput s st
            writeBack' st (NoOp)           = return st
            writeBack' st (Terminate)      = Exit st

-- Increment PC by 1.
incPc :: State -> Res State
incPc st = do
    pc <- regVal (pcIdx st) st
    setRegVal (pcIdx st) (pc+1) st

-- Return whether the pipeline should stall to wait for branch instructions
-- to be executed, i.e. if there are branch instructions in the fetch or decode
-- stages. Do not need to check for execute stage because write-back results
-- are available via bypass.
shouldStall :: Pipeline -> Bool
shouldStall p = f || d where
    f  = maybe False isBranch (fmap snd (fetched p))
    d  = maybe False isBranch (fmap snd (decoded p))

-- Shifts instructions through pipeline.
advancePipeline :: Maybe (ROBIdx, Instr) -> State -> Pipeline -> Res (State, Pipeline)
advancePipeline fetched st1 p = do
    -- TODO: Subsitute this with exec that updates state when RS implemented.
    let executer = \i st -> fmap (\wb -> (wb, st)) (trace ("EXEC:\n" ++ show st ++ "\n") $ exec i st)
    (st2, p') <- P.advance (fetched, st1) decode executer Exec.commit writeBack p
    return (st2, p')

-- Returns state which contains bypass value that was just written as part of
-- the write-back stage of the pipeline. This makes this value available to
-- previous stages of the pipeline.
bypassed :: State -> Pipeline -> State
bypassed st p = St.withBypass b st where
    b = BP.fromPipeline p

-- Shift instructions through pipeline, fetching a new instruction on each cycle.
cycle :: State -> Pipeline -> Res (State, Pipeline)
cycle st1 p = do
    (fetched, st2) <- fetch st1
    (st3, p') <- advancePipeline fetched st2 p
    st4 <- incPc st3
    return (bypassed st4 p', p')

-- Shift instructions through pipeline without fetching a new instruction.
-- PC is also NOT updated.
cycleStall :: State -> Pipeline -> Res (State, Pipeline)
cycleStall st1 p = do
    (st2, p') <- advancePipeline Nothing st1 p
    return (bypassed st2 p', p')

-- Run Res to completion, i.e. until exit system call occurs.
runPipeline :: State -> Pipeline -> Res (State, Pipeline)
runPipeline st p = trace ("PIPE: " ++ show p ++ "\n") $ do
    let x = if not (shouldStall p)
                then Exec.cycle st p
                else Exec.cycleStall st p
    (st', p') <- x
    runPipeline (St.incCycles st') p'

-- Run Res to completion starting with an empty pipeline.
run :: State -> State
run st =
    case runPipeline st P.empty of
        Exit st    -> st
        Crash e st -> error (show e ++ "\n" ++ show st)
        Res x       -> error ("Did not terminate:" ++ show x)
