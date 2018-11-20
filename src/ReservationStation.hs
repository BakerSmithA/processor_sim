module ReservationStation where

import Instr (Addr, RegIdx, Val)

-- The location an operation is waiting for data from.
data Src = Mem Addr
         | Reg RegIdx

-- Entry in reservation station. Details operation that is waiting for operands.
data Entry a = Waiting1 (Val -> a)        Src -- Waiting for an operand (left or right).
             | Waiting2 (Val -> Val -> a) Src -- Waiting for both operands.

-- Reservation station, i.e. table of operations that are waiting for operands.
newtype RS a = RS [Entry a]
