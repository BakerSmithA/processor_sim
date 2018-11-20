module ReservationStation where

import Instr (Addr, RegIdx, Val)
import Bypass

-- The location an operation is waiting for data from.
data Src = Mem Addr
         | Reg RegIdx

-- Entry in reservation station. Details operation that is waiting for operands.
data Waiting = WaitingL Src Val -- Binary operation waiting for left operand, and right has already been supplied.
             | WaitingR Val Src -- Binary operation waiting for right operand, and left has already been supplied.
             | WaitingB Src Src -- Binary operation waiting for both operands.
             | WaitingU Src     -- Unary operation waiting for single operand.

-- Operation that has had all operands 'filled in' and is ready to be performed.
data FilledOp = UniOp Val
              | BinOp Val Val

-- Reservation station, i.e. table of operations that are waiting for operands.
newtype RS = RS [Waiting]

-- Attempt to 'fill in' value in operation that is waiting for operands.
fill :: Waiting -> Bypass -> Either Waiting FilledOp
fill (WaitingL src _) b = undefined
fill (WaitingR _ src) b = undefined
fill (WaitingB s1 s2) b = undefined
fill (WaitingU src)   b = undefined

-- Check whether the source for an operand, that is being waiting, for can be
-- fullfilled by the value passed via common data bus.
checkSrc :: Src -> Bypass -> Maybe Val
checkSrc (Mem addr) (BypassMem ckAddr val) = check addr ckAddr val
checkSrc (Reg idx)  (BypassReg ckIdx  val) = check idx ckIdx val
checkSrc _ _ = Nothing

check :: (Eq a) => a -> a -> b -> Maybe b
check x y v | x == y = Just v
            | otherwise = Nothing
