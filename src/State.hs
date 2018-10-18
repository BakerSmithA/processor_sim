module State where

import Mem (Mem)
import qualified Mem as Mem
import Reg (RegFile, RegIdx)
import qualified Reg as Reg
import Instr

-- Stores current state of virtual machine.
-- Uses Von Newmann architecture, and so data and instructions are separate.
data State = State {
    -- Memory
    mem    :: Mem Addr Val
  , regs   :: RegFile
  , instrs :: Mem Addr Instr
    -- Register indices
  , pcIdx  :: RegIdx -- Program Counter
  , spIdx  :: RegIdx -- Stack Pointer
  , lrIdx  :: RegIdx -- Link Register
  , bpIdx  :: RegIdx -- Base Pointer
  , retIdx :: RegIdx -- Return value register (EAX in x86)
    -- Output
  , output :: String
}

instance Show State where
    show vm = "INSTR:\n\t" ++ (show $ pc vm) ++ ": " ++ show (Mem.loadSafe (instrs vm) (pc vm)) ++ "\n"
           ++ "MEM:\n\t"   ++ show (mem vm) ++ "\n"
           ++ "REG:\n\t"   ++ show (regs vm) ++ "\n"

-- Runs instructions until the pc points past the instructions.
runAll :: State -> [State]
runAll vm | (pc vm) > Mem.maxAddr (instrs vm) = [vm]
          | otherwise = vm : runAll (next vm)

run :: State -> State
run vm | (pc vm) > Mem.maxAddr (instrs vm) = vm -- End of instructions.
       | otherwise = run (next vm)

-- Returns address of current instruction.
pc :: State -> Val
pc vm = reg (pcIdx vm) vm

-- Advances the state of the State by executing the instruction pointed to by the pc.
next :: State -> State
next vm = exec instr vm where
    instr = Mem.load (instrs vm) (pc vm)

-- Executes instruction and advances program counter.
exec :: Instr -> State -> State
-- Memory
exec (MoveI r val)                 vm = inc $ vm { regs = Reg.write (regs vm) r val }
exec (Move r from)                 vm = inc $ vm { regs = Reg.write (regs vm) r (reg from vm) }
exec (LoadIdx r base offset)       vm = load  r (reg base vm + offset) vm
exec (LoadBaseIdx r base rOffset)  vm = load  r (reg base vm + reg rOffset vm) vm
exec (StoreIdx r base offset)      vm = store r (reg base vm + offset) vm
exec (StoreBaseIdx r base rOffset) vm = store r (reg base vm + reg rOffset vm) vm
-- Arithmetic/Logic
exec (Add r x y)  vm = op r x (+) (reg y vm) vm
exec (AddI r x i) vm = op r x (+) i vm
exec (Sub r x y)  vm = op r x (-) (reg y vm) vm
exec (SubI r x i) vm = op r x (-) i vm
exec (Mult r x y) vm = op r x (*) (reg y vm) vm
exec (Eq r x y)   vm = op r x eqVal (reg y vm) vm
exec (Lt r x y)   vm = op r x ltVal (reg y vm) vm
exec (Or r x y)   vm = op r x orVal (reg y vm) vm
exec (And r x y)  vm = op r x andVal (reg y vm) vm
exec (Not r x)    vm = inc $ vm { regs = Reg.write (regs vm) r (notVal (reg x vm)) }
-- Control
exec (B addr)    vm = vm { regs = Reg.write (regs vm) (pcIdx vm) addr }
exec (BT r addr) vm = if reg r vm == 1
                          then vm { regs = Reg.write (regs vm) (pcIdx vm) addr }
                          else inc vm
exec (Ret)       vm = vm { regs = Reg.write (regs vm) (pcIdx vm) (reg (lrIdx vm) vm) }
exec (Print r)   vm = inc $ vm { output = (output vm) ++ show (reg r vm) }
exec (PrintLn)   vm = inc $ vm { output = (output vm) ++ "\n" }

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

-- Advances instruction pointer by 1.
inc :: State -> State
inc vm = vm { regs = Reg.write (regs vm) pc (reg pc vm + 1) } where
    pc = pcIdx vm

-- Returns contents of register.
reg :: RegIdx -> State -> Val
reg r vm = Reg.read (regs vm) r

-- Loads contents of memory at address into register.
load :: RegIdx -> Addr -> State -> State
load r addr vm = inc $ vm { regs = Reg.write (regs vm) r memVal } where
    memVal = Mem.load (mem vm) addr

-- Stores contents of register into memory at address.
store :: RegIdx -> Addr -> State -> State
store r addr vm = inc $ vm { mem = Mem.store (mem vm) addr (reg r vm) }

op :: RegIdx -> RegIdx -> (Val -> Val -> Val) -> Val -> State -> State
op r x operation y vm = inc $ vm { regs = Reg.write (regs vm) r result } where
    result = operation (reg x vm) y
