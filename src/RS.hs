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
