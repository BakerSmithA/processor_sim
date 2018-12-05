module RS where

import Instr

-- Reservation station.
newtype RS = RS [RSInstr]

-- tryFill :: RS -> ([RS
