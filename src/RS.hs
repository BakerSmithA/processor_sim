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

rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just

-- Returns the memory address that a load instruction will access.
addrLoad :: MemInstr d (Either PhyReg Val) -> Maybe Val
addrLoad (LoadIdx     _ b off) = do
    b' <- rightToMaybe b
    return (b' + fromIntegral off)
addrLoad (LoadBaseIdx _ b off) = do
    b' <- rightToMaybe b
    off' <- rightToMaybe off
    return (b' + off')
addrLoad _ = error "Tried to get address of non-load"

-- Tries to fill in operands of store instruction, or goes to memory to retrieve
-- load the value at an address for a load instruction.
fillMem :: (Monad m) => (PhyReg -> m (Maybe Val)) -> (Val -> m Val) -> RS RSMemInstr -> m (RS RSMemInstr)
fillMem regVal memVal = fill $ \instr -> mapMemM (fillRegDst instr) (fillRegSrc regVal) instr where
    -- Fills in destination register with loaded value from memory, and physical
    -- register to store the value in.
    -- fillRegDst :: (PhyReg, Maybe Val) -> m (PhyReg, Maybe Val)
    fillRegDst instr (phy, maybeVal) =
        case maybeVal of
            -- No need to fetch the value if it's already been retrieved.
            Just val -> return (phy, Just val)
            -- Fetch the value from memory if it has not been retrieved.
            Nothing ->
                case addrLoad instr of
                    -- Address is not ready yet, i.e. base and offset operands
                    -- are not filled in.
                    Nothing   -> undefined --return (phy, Nothing)
                    Just addr -> undefined --memVal addr >>= \val -> return (phy, Just val)

-- Tries to fill in operands in an ALU instruction.
fillAL :: (Monad m) => (PhyReg -> m (Maybe Val)) -> RS RSALInstr -> m (RS RSALInstr)
fillAL regVal = fill $ \instr -> mapALM return (fillRegSrc regVal) instr

-- Tries to fill in operands in a branch instruction.
fillB :: (Monad m) => (PhyReg -> m (Maybe Val)) -> RS RSBranchInstr -> m (RS RSBranchInstr)
fillB regVal = fill $ \instr -> mapBM (fillRegSrc regVal) instr

-- Tries to fill in operands in an output instruction.
fillOut :: (Monad m) => (PhyReg -> m (Maybe Val)) -> RS RSOutInstr -> m (RS RSOutInstr)
fillOut regVal = fill $ \instr -> mapOutM (fillRegSrc regVal) instr
