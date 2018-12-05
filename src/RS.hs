module RS where

import Instr
import State as St

-- Reservation station.
type RS = [RSInstr]

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
            return $ case maybeVal of
                Nothing  -> Left phy
                Just val -> Right val

-- Removes instructions that have had all operands 'fill in'.
-- Performs additional check given, and only promotes if returns true.
promote :: (EInstr -> Bool) -> RS -> ([EInstr], RS)
promote cond = foldr checkDone ([], []) where
    checkDone :: RSInstr -> ([EInstr], RS) -> ([EInstr], RS)
    checkDone rsInstr (execIs, rs) =
        case rsToExec rsInstr of
            Nothing        -> (execIs, rsInstr:rs)
            Just execInstr -> if cond execInstr 
                                  then (execInstr:execIs, rs)
                                  else (execIs, rsInstr:rs)

-- Attempts to convert an instruction stored in a reservation station to an
-- executed instruction. Can occur if the RS instruction has had all operands
-- filled in.
rsToExec :: RSInstr -> Maybe EInstr
rsToExec = undefined
