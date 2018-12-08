module RS where

import Instr
import Types

-- Reservation station containing instructions with possibly filled operands.
type RS a = [a]

fromList :: [a] -> RS a
fromList = id

empty :: RS a
empty = []

add :: a -> RS a -> RS a
add = (:)

-- Tries to fill in operands of instructions in the reservation station.
fill :: (Monad m) => (a -> m a) -> RS a -> m (RS a)
fill fillOp = mapM fillOp

-- Removes instruction that have had all their operands filled and are ready.
promote :: (a -> Maybe b) -> RS a -> ([b], RS a)
promote promoteInstr = foldr checkDone ([], []) where
    checkDone instr (outs, rs) =
        case promoteInstr instr of
            Nothing  -> (outs, instr:rs)
            Just out -> (out:outs, rs)

-- Fills in a source register in an operand by getting the value from a register,
-- or does nothing if the value is not available.
fillRegSrc :: (Monad m) => (PhyReg -> m (Maybe Val)) -> Either PhyReg Val -> m (Either PhyReg Val)
fillRegSrc regVal = either f (return . Right) where
    f phy = do
        maybeVal <- regVal phy
        return $ case maybeVal of
            Nothing  -> Left phy
            Just val -> Right val

-- Tries to fill in operands in an ALU instruction.
fillAL :: (Monad m) => (PhyReg -> m (Maybe Val)) -> RS RSALInstr -> m (RS RSALInstr)
fillAL regVal = fill $ \instr -> mapALM return (fillRegSrc regVal) instr

-- Tries to fill in operands in a branch instruction.
fillB :: (Monad m) => (PhyReg -> m (Maybe Val)) -> RS RSBranchInstr -> m (RS RSBranchInstr)
fillB regVal = fill $ \instr -> mapBM (fillRegSrc regVal) instr

-- Tries to fill in operands in an output instruction.
fillOut :: (Monad m) => (PhyReg -> m (Maybe Val)) -> RS RSOutInstr -> m (RS RSOutInstr)
fillOut regVal = fill $ \instr -> mapOutM (fillRegSrc regVal) instr
