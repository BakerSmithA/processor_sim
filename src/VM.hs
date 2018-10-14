module VM where

import Mem (Mem)
import qualified Mem as Mem
import Reg (RegFile, RegIdx)
import qualified Reg as Reg
import Instr
import Debug.Trace

-- Stores current state of virtual machine.
-- Uses Von Newmann architecture, and so data and instructions are separate.
data VM = VM {
    mem    :: Mem Addr Val
  , regs   :: RegFile
  , instrs :: Mem Addr Instr
    -- Register indices
  , pcIdx  :: RegIdx -- Program Counter
  , spIdx  :: RegIdx -- Stack Pointer
  , lrIdx  :: RegIdx -- Link Register
} deriving (Show)

-- Runs instructions until the pc points past the instructions.
run :: VM -> VM
run vm | (pc vm) == Mem.maxAddr (instrs vm) + 1 = vm -- End of instructions.
       | otherwise = run (next vm)

-- Returns address of current instruction.
pc :: VM -> Val
pc vm = reg (pcIdx vm) vm

-- Advances the state of the VM by executing the instruction pointed to by the pc.
next :: VM -> VM
next vm = exec instr vm where
    instr = Mem.load (instrs vm) (pc vm)

-- Executes instruction and advances program counter.
exec :: Instr -> VM -> VM
-- Memory
exec (MoveI r val)                 vm = inc $ vm { regs = Reg.write (regs vm) r val }
exec (Move r from)                 vm = inc $ vm { regs = Reg.write (regs vm) r (reg from vm) }
exec (LoadIdx r base offset)       vm = load  r (reg base vm + offset) vm
exec (LoadBaseIdx r base rOffset)  vm = load  r (reg base vm + reg rOffset vm) vm
exec (StoreIdx r base offset)      vm = store r (reg base vm + offset) vm
exec (StoreBaseIdx r base rOffset) vm = store r (reg base vm + reg rOffset vm) vm
-- Arithmetic/Logic
exec (Add r x y) vm = op r x (+) y vm
exec (Sub r x y) vm = op r x (-) y vm
exec (Eq r x y)  vm = op r x eqVal y vm
exec (Or r x y)  vm = op r x orVal y vm
exec (And r x y) vm = op r x andVal y vm
-- Control
exec (B addr)    vm = vm { regs = Reg.write (regs vm) (pcIdx vm) addr }
exec (BT r addr) vm = if reg r vm == 1
                          then vm { regs = Reg.write (regs vm) (pcIdx vm) addr }
                          else inc vm
exec (Ret)       vm = vm { regs = Reg.write (regs vm) (pcIdx vm) (reg (lrIdx vm) vm) }
exec (Print r)   vm = trace ("Reg " ++ show r ++ " = " ++ show (reg r vm)) $ inc vm

eqVal :: Val -> Val -> Val
eqVal x y | x == y    = 1
          | otherwise = 0

orVal :: Val -> Val -> Val
orVal x y | x == 1 || y == 1 = 1
          | otherwise        = 0

andVal :: Val -> Val -> Val
andVal x y | x == 1 && y == 1 = 1
           | otherwise        = 0

-- Advances instruction pointer by 1.
inc :: VM -> VM
inc vm = vm { regs = Reg.write (regs vm) pc (reg pc vm + 1) } where
    pc = pcIdx vm

-- Returns contents of register.
reg :: RegIdx -> VM -> Val
reg r vm = Reg.read (regs vm) r

-- Loads contents of memory at address into register.
load :: RegIdx -> Addr -> VM -> VM
load r addr vm = inc $ vm { regs = Reg.write (regs vm) r memVal } where
    memVal = Mem.load (mem vm) addr

-- Stores contents of register into memory at address.
store :: RegIdx -> Addr -> VM -> VM
store r addr vm = inc $ vm { mem = Mem.store (mem vm) addr (reg r vm) }

op :: RegIdx -> RegIdx -> (Val -> Val -> Val) -> RegIdx -> VM -> VM
op r x operation y vm = inc $ vm { regs = Reg.write (regs vm) r result } where
    result = operation (reg x vm) (reg y vm)
