module VM where

import Mem (Mem, Word32)
import qualified Mem as Mem (load)
import Reg (RegFile)
import qualified Reg as Reg (write)
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

-- Advances the state of the VM if the end of the instructions have not
-- been reached. Otherwise returns Nothing.
next :: VM -> VM
next = undefined

exec :: Instr -> VM -> VM
exec (LoadIdx r base offset) vm = vm { regs = Reg.write (regs vm) r memVal } where
    memVal = Mem.load (mem vm) (base + offset)
