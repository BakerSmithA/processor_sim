module ReservationStation where

import Instr (Addr, RegIdx, Val)
import Bypass

-- The location an operation is waiting for data from.
data Src = Mem Addr
         | Reg RegIdx

-- Entry in reservation station. Details operation that is waiting for operands.
data Waiting = WaitingU Src     -- Unary operation waiting for single operand.
             | WaitingL Src Val -- Binary operation waiting for left operand, and right has already been supplied.
             | WaitingR Val Src -- Binary operation waiting for right operand, and left has already been supplied.
             | WaitingB Src Src -- Binary operation waiting for both operands.

-- Operation that has had all operands 'filled in' and is ready to be performed.
data FilledOp = UniOp Val
              | BinOp Val Val

-- Reservation station, i.e. table of operations that are waiting for operands.
newtype RS = RS [Waiting]

-- Attempt to 'fill in' value in operation that is waiting for operands.
fill :: Waiting -> Bypass -> Either Waiting FilledOp
fill w@(WaitingU src)    b = maybe (Left w) (\v -> Right $ UniOp v) (checkSrc src b)
fill w@(WaitingL src vr) b = maybe (Left w) (\vl -> Right $ BinOp vl vr) (checkSrc src b)
fill w@(WaitingR vl src) b = maybe (Left w) (\vr -> Right $ BinOp vl vr) (checkSrc src b)
fill w@(WaitingB sl sr)  b =
    -- Check whether left value can be filled in.
    case (checkSrc sl b) of
        Just vl -> fill (WaitingR vl sr) b
        Nothing ->
            -- Check whether right value can be filled in.
            case (checkSrc sr b) of
                Just vr -> fill (WaitingL sl vr) b
                Nothing -> Left w

-- Check whether the source for an operand, that is being waiting, for can be
-- fullfilled by the value passed via common data bus.
checkSrc :: Src -> Bypass -> Maybe Val
checkSrc (Mem addr) (BypassMem ckAddr val) = check addr ckAddr val
checkSrc (Reg idx)  (BypassReg ckIdx  val) = check idx ckIdx val
checkSrc _ _ = Nothing

check :: (Eq a) => a -> a -> b -> Maybe b
check x y v | x == y = Just v
            | otherwise = Nothing
