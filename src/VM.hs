module VM where

import Mem (Mem, Addr, Word32)
import qualified Mem as Mem
import Reg (RegFile, RegIdx)
import qualified Reg as Reg
import Instr

-- Stores current state of virtual machine.
-- Uses Von Newmann architecture, and so data and instructions are separate.
data VM = VM {
    mem    :: Mem Word32
  , regs   :: RegFile
  , instrs :: Mem Instr
    -- Register indices
  , pcIdx  :: Word32 -- Program Counter
  , spIdx  :: Word32 -- Stack Pointer
  , lrIdx  :: Word32 -- Link Register
}

-- Runs instructions until the pc points past the instructions.
run :: VM -> VM
run = undefined

-- Advances the state of the VM by executing the instruction pointed to by the pc.
next :: VM -> VM
next vm = exec instr vm where
    instr = Mem.load (instrs vm) instrAddr
    instrAddr = reg (pcIdx vm) vm

-- Executes instruction and advances program counter.
exec :: Instr -> VM -> VM
-- Memory
exec (MoveI r val)                 vm = inc $ vm { regs = Reg.write (regs vm) r val }
exec (LoadIdx r base offset)       vm = load  r (base + offset) vm
exec (LoadBaseIdx r base rOffset)  vm = load  r (base + reg rOffset vm) vm
exec (StoreIdx r base offset)      vm = store r (base + offset) vm
exec (StoreBaseIdx r base rOffset) vm = store r (base + reg rOffset vm) vm
-- Arithmetic/Logic
exec (Add r x y)  vm = op r x (+) (reg y vm) vm
exec (AddI r x i) vm = op r x (+) i vm
exec (Sub r x y)  vm = op r x (-) (reg y vm) vm
exec (SubI r x i) vm = op r x (-) i vm
-- Control
exec (B addr)     vm = vm { regs = Reg.write (regs vm) (pcIdx vm) addr }
exec (BLT r addr) vm = if reg r vm > 0
                           then vm { regs = Reg.write (regs vm) (pcIdx vm) addr }
                           else inc vm
exec (Ret)        vm = vm { regs = Reg.write (regs vm) (pcIdx vm) (lrIdx vm) }

-- Advances instruction pointer by 1.
inc :: VM -> VM
inc vm = vm { regs = Reg.write (regs vm) pc (reg pc vm + 1) } where
    pc = pcIdx vm

-- Returns contents of register.
reg :: RegIdx -> VM -> Word32
reg r vm = Reg.read (regs vm) r

-- Loads contents of memory at address into register.
load :: RegIdx -> Addr -> VM -> VM
load r addr vm = inc $ vm { regs = Reg.write (regs vm) r memVal } where
    memVal = Mem.load (mem vm) addr

-- Stores contents of register into memory at address.
store :: RegIdx -> Addr -> VM -> VM
store r addr vm = inc $ vm { mem = Mem.store (mem vm) addr (reg r vm) }

op :: RegIdx -> RegIdx -> (Word32 -> Word32 -> Word32) -> Word32 -> VM -> VM
op r x operation y vm = inc $ vm { regs = Reg.write (regs vm) r result } where
    result = operation (reg x vm) y
