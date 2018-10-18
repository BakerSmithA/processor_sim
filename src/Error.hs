module Error where

import Instr (RegIdx, Addr)

data Error
    -- Tried to access a register with an invalid index.
    = RegOutOfRange RegIdx
    -- Tried to access memory with an invalid index.
    | MemOutOfRange Addr
