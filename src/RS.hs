module RS where

import Instr (Addr, RegIdx, Val)
import Bypass (Bypass(..))

-- The location an operation is waiting for data from.
data Src = Mem Addr
         | Reg RegIdx
         deriving (Eq, Show)

-- Entry in reservation station. Details operation that is waiting for operands.
data Waiting = WaitingU Src     -- Unary operation waiting for single operand.
             | WaitingL Src Val -- Binary operation waiting for left operand, and right has already been supplied.
             | WaitingR Val Src -- Binary operation waiting for right operand, and left has already been supplied.
             | WaitingB Src Src -- Binary operation waiting for both operands.
             deriving (Eq, Show)

-- Operation that has had all operands 'filled in' and is ready to be performed.
data FilledOp = UniOp Val
              | BinOp Val Val
              deriving (Eq, Show)

-- Reservation station, i.e. table of operations that are waiting for operands.
newtype RS a = RS [(a, Waiting)]
             deriving (Eq, Show)

fromList :: [(a, Waiting)] -> RS a
fromList = RS

-- Reservation station containing no entries.
empty :: RS a
empty = RS []

-- Adds an operation that is waiting for operands to reservation station.
addOp :: (a, Waiting) -> RS a -> RS a
addOp w (RS ws) = RS (w:ws)

-- Attempts to fill in operands of operations waiting in reservation station.
-- Returns any operations that have all their operands fill, and new state of
-- reservation station.
fill :: Bypass -> RS a -> ([FilledOp], RS a)
fill b (RS ws) = foldr f ([], empty) ws where
    f (x, w) (ops, rs) = either waiting filled (fillOp w b) where
        waiting w' = (ops, addOp (x, w') rs)
        filled op  = (op:ops, rs)

-- Attempt to 'fill in' value in operation that is waiting for operands.
fillOp :: Waiting -> Bypass -> Either Waiting FilledOp
fillOp w@(WaitingU src)    b = maybe (Left w) (\v -> Right $ UniOp v) (checkSrc src b)
fillOp w@(WaitingL src vr) b = maybe (Left w) (\vl -> Right $ BinOp vl vr) (checkSrc src b)
fillOp w@(WaitingR vl src) b = maybe (Left w) (\vr -> Right $ BinOp vl vr) (checkSrc src b)
fillOp w@(WaitingB sl sr)  b =
    -- Check whether left value can be filled in.
    case (checkSrc sl b) of
        Just vl -> fillOp (WaitingR vl sr) b
        Nothing ->
            -- Check whether right value can be filled in.
            case (checkSrc sr b) of
                Just vr -> fillOp (WaitingL sl vr) b
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
