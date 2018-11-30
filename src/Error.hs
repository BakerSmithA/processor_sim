module Error where

import Instr (RegIdx, Addr, Val)
import RRT (PhyReg)

type InstrAddr = Val

data Error
    -- Tried to access a register with an invalid index.
    = RegOutOfRange { regIdx :: RegIdx, pc :: InstrAddr }
    -- Tried to access memory with an invalid address.
    | MemOutOfRange { memAddr :: Addr, pc :: InstrAddr }
    -- Tried to access instruction with an invalid address.
    | InstrOutOfRange { instrAddr :: Addr, pc :: InstrAddr }
    -- Tried to free a physical register that was not assigned to a register name.
    | FreeNonExistentPhyReg { phyReg :: PhyReg, pc :: InstrAddr }
    deriving (Eq, Show)
