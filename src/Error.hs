module Error where

import Instr (RegIdx, PhyReg, Addr, Val)

type InstrAddr = Val

data Error
    -- Tried to access a register with an invalid index.
    = RegOutOfRange { phyReg :: PhyReg, pc :: InstrAddr }
    -- Tried to access memory with an invalid address.
    | MemOutOfRange { memAddr :: Addr, pc :: InstrAddr }
    -- Tried to access instruction with an invalid address.
    | FInstrOutOfRange { instrAddr :: Addr, pc :: InstrAddr }
    -- Trying to use a register which has not been mapped to a physical register.
    | NoPhyRegAssigned { regIdx :: RegIdx, pc :: InstrAddr }
    -- Tried to free a physical register that was not assigned to a register name.
    | FreeNonExistentPhyReg { phyReg :: PhyReg, pc :: InstrAddr }
    -- Tried to allocate a physical register when there are none free. Should
    -- stall if there are none free.
    | NoFreePhyRegs { pc :: InstrAddr }
    deriving (Eq, Show)
