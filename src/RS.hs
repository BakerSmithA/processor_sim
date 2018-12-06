module RS where

import Instr
import State as St

-- Reservation station.
type RS = [RSInstr]

-- Add a decoded instruction to the reservation station.
-- Once all operands are available the instruction will be promoted from the RS.
-- When put in the RS, none of the operands are marked as 'filled in'.
add :: DInstr -> RS -> RS
add = (:) . mapI id Left id

-- Iterates through instructions in reservation station and tries to 'fill in'
-- missing operands.
tryFill :: State -> RS -> Res RS
tryFill st = mapM fill where
    fill :: RSInstr -> Res RSInstr
    fill = mapIM return fillRSrc return

    -- Fills in the source register in an instruction with a value read from
    -- the state's registers, bypass, reorder-buffer, etc.
    fillRSrc :: Either PhyReg Val -> Res (Either PhyReg Val)
    fillRSrc = either f (return . Right) where
        f :: PhyReg -> Res (Either PhyReg Val)
        f phy = do
            maybeVal <- St.regVal phy st
            return $ case (Just maybeVal) of -- TODO: Get maybe from reg val instead of Just.
                Nothing  -> Left phy
                Just val -> Right val

-- Removes instructions that have had all operands 'fill in'.
-- Performs additional check given, and only promotes if returns true.
promote :: (EInstr -> Bool) -> RS -> ([EInstr], RS)
promote cond = foldr checkDone ([], []) where
    checkDone :: RSInstr -> ([EInstr], RS) -> ([EInstr], RS)
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
