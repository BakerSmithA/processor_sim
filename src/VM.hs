module VM where

import Mem (Mem, Word32)
import Reg (RegFile)
import Instr

-- Stores current state of virtual machine.
-- Uses Von Newmann architecture, and so data and instructiond are separate.
data VM = VM {
    mem    :: Mem Word32,
    regs   :: RegFile,
    instrs :: Mem Instr
}
