module RS where

import Instr
import State as St

-- Reservation station.
newtype RS = RS [RSInstr]

-- Iterates through instructions in reservation station and tries to 'fill in'
-- missing operands.
tryFill :: State -> RS -> RS
tryFill = undefined

-- tryFill st (RS is) = RS (map fill is) where
--     fill = mapI id fillRSrc fillAddr
--
--     fillRSrc r =
--         case St.regVal
