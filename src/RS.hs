module RS where

import Types
import Instr

-- Reservation station.
type RS = [RSInstr]

-- Empty reservation station with no entries.
empty :: RS
empty = []

-- Return RS contaning given entries.
fromList :: [RSInstr] -> RS
fromList = id

-- Add a decoded instruction to the reservation station.
-- Once all operands are available the instruction will be promoted from the RS.
-- When put in the RS, none of the operands are marked as 'filled in'.
add :: DInstr -> RS -> RS
add = (:) . mapI id Left id

-- Iterates through instructions in reservation station and tries to 'fill in'
-- missing operands. Given a function to get the value of register, or Nothing
-- if the value is invalid.
tryFill :: (Monad m) => (PhyReg -> m (Maybe Val)) -> RS -> m RS
tryFill regVal = mapM fill where
    fill = mapIM return fillRSrc return

    -- Fills in the source register in an instruction with a value read from
    -- the state's registers, bypass, reorder-buffer, etc.
    fillRSrc = either f (return . Right) where
        f phy = do
            maybeVal <- regVal phy
            return $ case maybeVal of
                Nothing  -> Left phy
                Just val -> Right val

-- Removes instructions that have had all operands 'fill in'.
-- Performs additional check given, and only promotes if returns true.
promote :: (EInstr -> Bool) -> RS -> ([EInstr], RS)
promote cond = foldr checkDone ([], []) where
    checkDone rsInstr (execIs, rs) =
        case checkFilled rsInstr of
            Nothing        -> (execIs, rsInstr:rs)
            Just execInstr -> if cond execInstr
                                  then (execInstr:execIs, rs)
                                  else (execIs, rsInstr:rs)

-- Checks whether an entry in the reservation station has all operands filled,
-- and if so returns instruction containing filled values.
checkFilled :: RSInstr -> Maybe EInstr
checkFilled = mapIM return f return where
    f = either (const Nothing) Just
