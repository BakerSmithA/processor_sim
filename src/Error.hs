module Error where

import Instr (RegIdx, Addr, Val)

type InstrAddr = Val

data Error
    -- Tried to access a register with an invalid index.
    = RegOutOfRange { regIdx :: RegIdx, pcVal :: InstrAddr }
    -- Tried to access memory with an invalid address.
    | MemOutOfRange { memAddr :: Addr, pcVal :: InstrAddr }
    -- Tried to access instruction with an invalid address.
    | InstrOutOfRange { instrAddr :: Addr, pcVal :: InstrAddr }
    deriving (Eq, Show)
