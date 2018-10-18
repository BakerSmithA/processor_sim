module VM where

import State
import Error
import Instr (RegIdx, Addr, Val)
import qualified Mem as Mem
import qualified Mem as Reg

-- Current state of the virtual machine, or whether it crashed, e.g. by
-- accessing memory index that is out of bounds.
data VM a = VM a
          | End
          | Crash Error

instance Functor VM where
    -- fmap :: (a -> b) -> f a -> f b
    fmap f (VM x)    = VM (f x)
    fmap f (End)     = End
    fmap _ (Crash e) = Crash e

instance Applicative VM where
    -- pure :: a -> VM a
    pure = VM
    -- (<*>) :: f (a -> b) -> f a -> f b
    (VM f)    <*> vm = fmap f vm
    (End)     <*> _ = End
    (Crash e) <*> _ = Crash e

instance Monad VM where
    -- (>>=) :: m a -> (a -> m b) -> m b
    (VM x)    >>= f  = f x
    (End)     >>= _ = End
    (Crash e) >>= _ = Crash e

-- Return value of a register, or Crash if invalid index.
regVal :: RegIdx -> State -> VM Val
regVal i st =
    case Reg.load i (regs st) of
        Nothing  -> Crash (RegOutOfRange i)
        Just val -> return val

-- Set the value stored in a register, or Crash if invalid index.
setRegVal :: RegIdx -> Val -> State -> VM State
setRegVal i val st =
    case Reg.store i val (regs st) of
        Nothing   -> Crash (RegOutOfRange i)
        Just regs -> return st { regs = regs }

-- Returns value of an address in memory, or Crash if invalid address.
memVal :: Addr -> State -> VM Val
memVal i st =
    case Mem.load i (mem st) of
        Nothing  -> Crash (MemOutOfRange i)
        Just val -> return val

-- Set the value at a memory address, or Crash if invalid address.
setMemVal :: Addr -> Val -> State -> VM State
setMemVal i val st =
    case Mem.store i val (mem st) of
        Nothing  -> Crash (MemOutOfRange i)
        Just mem -> return st { mem = mem }

-- Increments the PC by 1, or returns End if at the last instruction.
inc :: State -> VM State
inc st = do
    pc <- regVal (pcIdx st) st
    if pc > Mem.maxAddr (instrs st)
        then End
        else setRegVal (pcIdx st) (pc + 1) st

-- Loads contents of memory at address into register.
load :: Addr -> RegIdx -> State -> VM State
load addr r st = do
    val <- regVal r st
    setMemVal addr val st >>= inc

-- Stores contents of register into memory address.
store :: RegIdx -> Addr -> State -> VM State
store r addr st = do
    val <- memVal addr st
    setRegVal r val st >>= inc
