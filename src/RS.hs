module RS where

import Types
import Instr

-- Reservation station.
type RS = [RSPipeInstr]

-- Empty reservation station with no entries.
empty :: RS
empty = []

-- Return RS contaning given entries.
fromList :: [RSPipeInstr] -> RS
fromList = id

-- Add a decoded instruction to the reservation station.
-- Once all operands are available the instruction will be promoted from the RS.
-- When put in the RS, none of the operands are marked as 'filled in'.
add :: DPipeInstr -> RS -> RS
add = (:) . mapPipeI id Left

-- Tries to fill in missing operands using provided function, and promotes and
-- instructions with all operands filled out of the RS.
run :: (Monad m) => (PhyReg -> m (Maybe Val)) -> (EPipeInstr -> Bool) -> RS -> m ([EPipeInstr], RS)
run regVal canPromote rs1 = do
    rs2 <- tryFill regVal rs1
    return (promote canPromote rs2)

-- Iterates through instructions in reservation station and tries to 'fill in'
-- missing operands. Given a function to get the value of register, or Nothing
-- if the value is invalid.
tryFill :: (Monad m) => (PhyReg -> m (Maybe Val)) -> RS -> m RS
tryFill regVal = mapM fill where
    fill = mapPipeIM return fillRSrc

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
promote :: (EPipeInstr -> Bool) -> RS -> ([EPipeInstr], RS)
promote canPromote = foldr checkDone ([], []) where
    checkDone rsInstr (execIs, rs) =
        case checkFilled rsInstr of
            Nothing        -> (execIs, rsInstr:rs)
            Just execInstr -> if canPromote execInstr
                                  then (execInstr:execIs, rs)
                                  else (execIs, rsInstr:rs)

-- Checks whether an entry in the reservation station has all operands filled,
-- and if so returns instruction containing filled values.
checkFilled :: RSPipeInstr -> Maybe EPipeInstr
checkFilled = mapPipeIM return f where
    f = either (const Nothing) Just
