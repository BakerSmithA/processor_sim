module VM where

import State
import Error
import Instr
import Pipeline
import qualified Mem as Mem
import qualified Mem as Reg

-- E.g. Mult, Add, And, Or, etc
type ValOp = (Val -> Val -> Val)

-- Current state of the virtual machine, or whether it crashed, e.g. by
-- accessing memory index that is out of bounds.
data VM a = VM a
          | End String -- Printed output of VM.
          | Crash Error

instance Functor VM where
    -- fmap :: (a -> b) -> f a -> f b
    fmap f (VM x)    = VM (f x)
    fmap f (End out) = End out
    fmap _ (Crash e) = Crash e

instance Applicative VM where
    -- pure :: a -> VM a
    pure = VM
    -- (<*>) :: f (a -> b) -> f a -> f b
    (VM f)    <*> vm = fmap f vm
    (End out) <*> _ = End out
    (Crash e) <*> _ = Crash e

instance Monad VM where
    -- (>>=) :: m a -> (a -> m b) -> m b
    (VM x)    >>= f = f x
    (End out) >>= _ = End out
    (Crash e) >>= _ = Crash e

-- Return value of a register, or Crash if invalid index.
regVal :: RegIdx -> State -> VM Val
regVal i st =
    case Reg.load i (regs st) of
        Nothing  -> Crash (RegOutOfRange i)
        Just val -> return val

-- Returns value of an address in memory, or Crash if invalid address.
memVal :: Addr -> State -> VM Val
memVal i st =
    case Mem.load i (mem st) of
        Nothing  -> Crash (MemOutOfRange i)
        Just val -> return val

-- Return instruction at address, or Crash if invalid address.
instrVal :: Addr -> State -> VM Instr
instrVal i st =
    case Mem.load i (instrs st) of
        Nothing    -> Crash (InstrOutOfRange i)
        Just instr -> return instr

-- Return value of PC.
pcVal :: State -> VM Addr
pcVal st = do
    pc <- regVal (pcIdx st) st
    return pc

-- Increments the PC by 1, or returns End if at the last instruction.
inc :: State -> VM State
inc st = do
    pc <- pcVal st
    setRegVal (pcIdx st) (pc + 1) st

-- Loads contents of memory at address into register.
load :: Addr -> RegIdx -> State -> VM WriteBackInstr
load addr r st = do
    val <- regVal r st
    return (WriteReg r val)

-- Stores contents of register into memory address.
store :: RegIdx -> Addr -> State -> VM WriteBackInstr
store r addr st = do
    val <- memVal addr st
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

-- Perform fetch state of pipeline by retreiving instruction.
fetch :: State -> VM Instr
fetch st = do
    pc <- pcVal st
    instrVal pc st

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
    val <- regVal r st
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
        Nothing   -> Crash (RegOutOfRange i)
        Just regs -> return st { regs = regs }

-- Set the value at a memory address, or Crash if invalid address.
setMemVal :: Addr -> Val -> State -> VM State
setMemVal i val st =
    case Mem.store i val (mem st) of
        Nothing  -> Crash (MemOutOfRange i)
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

-- Performs a cycle moving instructions one step through the pipeline.
-- cycle :: State -> Pipeline -> VM (State, Pipeline)
-- cycle = undefined
-- cycle st p = do
--     instr <- fetch st
--     d <- (fetched p) >>= decode
--     undefined
