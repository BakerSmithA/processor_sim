module VM where

import State
import Error
import Instr
import Pipeline as P
import qualified Mem as Mem
import qualified Mem as Reg
import Debug.Trace

-- E.g. Mult, Add, And, Or, etc
type ValOp = (Val -> Val -> Val)

-- Current state of the virtual machine, or whether it crashed, e.g. by
-- accessing memory index that is out of bounds.
data VM a = VM a
          | Crash Error State
          | Exit State
          deriving (Eq, Show)

instance Functor VM where
    -- fmap :: (a -> b) -> f a -> f b
    fmap f (VM x)       = VM (f x)
    fmap _ (Crash e st) = Crash e st
    fmap _ (Exit st)    = Exit st

instance Applicative VM where
    -- pure :: a -> VM a
    pure = VM
    -- (<*>) :: f (a -> b) -> f a -> f b
    (VM f)       <*> vm = fmap f vm
    (Crash e st) <*> _ = Crash e st
    (Exit st)    <*> _ = Exit st

instance Monad VM where
    -- (>>=) :: m a -> (a -> m b) -> m b
    (VM x)       >>= f = f x
    (Crash e st) >>= _ = Crash e st
    (Exit st)    >>= _ = Exit st

-- Return value of a register, or Crash if invalid index.
regVal :: RegIdx -> State -> VM Val
regVal i st =
    case Reg.load i (regs st) of
        Nothing  -> Crash (RegOutOfRange i) st
        Just val -> return val

-- Returns value of an address in memory, or Crash if invalid address.
memVal :: Addr -> State -> VM Val
memVal i st =
    case Mem.load i (mem st) of
        Nothing  -> Crash (MemOutOfRange i) st
        Just val -> return val

-- Loads contents of memory at address into register.
load :: Addr -> RegIdx -> State -> VM WriteBackInstr
load addr r st = do
    val <- memVal addr st
    return (WriteReg r val)

-- Stores contents of register into memory address.
store :: RegIdx -> Addr -> State -> VM WriteBackInstr
store r addr st = do
    val <- regVal r st
    return (WriteMem addr val)

-- Perform operation on value stored in register, and immediate value.
opI :: RegIdx -> RegIdx -> ValOp -> Val -> State -> VM WriteBackInstr
opI r x f imm st = do
    val <- regVal x st
    return (WriteReg r (f val imm))

-- Perform operation on two values stored in registers.
opReg :: RegIdx -> RegIdx -> ValOp -> RegIdx -> State -> VM WriteBackInstr
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

-- Perform fetch state of pipeline by retreiving instruction. Or, return Nothing
-- if the value of the pc is after the last instruction.
fetch :: State -> VM Instr
fetch st = do
    pc <- regVal (pcIdx st) st
    case Mem.load pc (instrs st) of
        Nothing    -> Crash (InstrOutOfRange pc) st
        Just instr -> return instr

-- Perform decode stage of pipeline.
decode :: Instr -> VM Instr
decode = return

-- Perform execution stage of pipeline, and generate instruction of what
-- to modify in machine.
exec :: Instr -> State -> VM WriteBackInstr
-- Memory
-- Move immediate value into register.
exec (MoveI r val) st =
    return (WriteReg r val)
-- Move value from one register into another.
exec (Move r from) st = do
    val <- regVal from st
    return (WriteReg r val)
-- Load value from memory into register, where reg and immediate provide
-- base and offset.
exec (LoadIdx r base offset) st = do
    baseAddr <- regVal base st
    load (baseAddr + offset) r st
-- Load value from memory into register, where two regs provide base and offset.
exec (LoadBaseIdx r base rOffset) st = do
    baseAddr <- regVal base st
    offsetAddr <- regVal rOffset st
    load (baseAddr + offsetAddr) r st
-- Store value from register into memory, where reg and immediate provide
-- base and offset.
exec (StoreIdx r base offset) st = do
    baseAddr <- regVal base st
    store r (baseAddr + offset) st
-- Store value from register into memory, where two regs provide base and offset.
exec (StoreBaseIdx r base rOffset) st = do
    baseAddr <- regVal base st
    offsetAddr <- regVal rOffset st
    store r (baseAddr + offsetAddr) st
-- Arithmetic/Logic
exec (Add r x y)  st = opReg r x (+) y st
exec (AddI r x i) st = opI r x (+) i st
exec (Sub r x y)  st = opReg r x (-) y st
exec (SubI r x i) st = opI r x (-) i st
exec (Mult r x y) st = opReg r x (*) y st
exec (Eq r x y)   st = opReg r x eqVal y st
exec (Lt r x y)   st = opReg r x ltVal y st
exec (Or r x y)   st = opReg r x orVal y st
exec (And r x y)  st = opReg r x andVal y st
exec (Not r x)    st = do
    val <- regVal x st
    return (WriteReg r (notVal val))
-- Branching
-- Unconditional branch to address.
exec (B addr) st =
    return (WriteReg (pcIdx st) addr)
-- Branch if value in register is true.
exec (BT r addr) st = do
    val <- regVal r st
    if val == 1
        then return (WriteReg (pcIdx st) addr)
        else return NoOp
-- Branch to value stored in link register.
exec (Ret) st = do
    addr <- regVal (lrIdx st) st
    return (WriteReg (pcIdx st) addr)
-- Terminate execution of the program.
exec (SysCall) st =
    return Terminate
-- Debugging.
-- Print value in register.
exec (Print r) st = do
    val <- regVal r st
    return (WritePrint (show val))
-- Print newline.
exec (PrintLn) st =
    return (WritePrint "\n")

-- Set the value stored in a register, or Crash if invalid index.
setRegVal :: RegIdx -> Val -> State -> VM State
setRegVal i val st =
    case Reg.store i val (regs st) of
        Nothing   -> Crash (RegOutOfRange i) st
        Just regs -> return st { regs = regs }

-- Set the value at a memory address, or Crash if invalid address.
setMemVal :: Addr -> Val -> State -> VM State
setMemVal i val st =
    case Mem.store i val (mem st) of
        Nothing  -> Crash (MemOutOfRange i) st
        Just mem -> return st { mem = mem }

-- Add string to output of VM.
addOutput :: String -> State -> VM State
addOutput s st = return st { output = (output st) ++ s }

-- Perform write-back stage of pipeline, writing result back to register/memory.
writeBack :: WriteBackInstr -> State -> VM State
writeBack (WriteReg r val) = setRegVal r val
writeBack (WriteMem i val) = setMemVal i val
writeBack (WritePrint s)   = addOutput s
writeBack (NoOp)           = return
writeBack (Terminate)      = Exit

-- Increment PC by 1.
incPc :: State -> VM State
incPc st = do
    pc <- regVal (pcIdx st) st
    setRegVal (pcIdx st) (pc+1) st

-- Performs a cycle moving instructions one step through the pipeline.
cycle :: State -> VM State
cycle st = do
    fetched  <- fetch st
    decoded  <- decode fetched
    executed <- exec decoded st
    st' <- writeBack executed st
    incPc st'

-- cycle :: State -> Pipeline -> VM (State, Pipeline)
-- cycle st p = do
--     let executer = (flip exec) st
--         writer   = (flip writeBack) st
--     fetched   <- fetch st
--     (st', p') <- advance fetched decode executer writer p
--     let st'' = maybe st id st'
--     incSt <- inc st''
--     return (incSt, p')

-- Run VM to completion until a SysCall is encountered.
run :: State -> State
run st =
    case VM.cycle st of
        Exit st'   -> st
        VM st'     -> run st'
        Crash e st -> error (show e ++ "\n" ++ show st)
