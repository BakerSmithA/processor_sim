module NewRS where

import Instr
import Types

type RegVal m = PhyReg -> m (Maybe Val)
type MemVal m = Addr   -> m Val

type RS a b = [Either a b]

type MemRS = RS RSMemInstr EMemInstr

-- Fills in operands of a memory instruction.
-- Goes to memory to load a value for load instructions.
fillMem :: (Monad m) => RegVal m -> MemVal m -> RSMemInstr -> m RSMemInstr
fillMem regVal memVal = fill where
    fill (LoadIdx (dst, val) base off) = do
        base' <- cachedRegVal regVal base
        val'  <- tryCachedMemVal memVal val base' (return off)
        return (LoadIdx (dst, val') base' off)

    fill (LoadBaseIdx (dst, val) base off) = do
        base' <- cachedRegVal regVal base
        off'  <- cachedRegVal regVal off
        val'  <- tryCachedMemVal memVal val base' off'
        return (LoadBaseIdx (dst, val') base' off')

    fill (StoreIdx src base off) = do
        src'  <- cachedRegVal regVal src
        base' <- cachedRegVal regVal base
        return (StoreIdx src' base' off)

    fill (StoreBaseIdx src base off) = do
        src'  <- cachedRegVal regVal src
        base' <- cachedRegVal regVal base
        off'  <- cachedRegVal regVal off
        return (StoreBaseIdx src' base' off')

-- If the value of an address has already been fetched, no action is performed.
-- Otherwise, if the address can be calculated (because both operands have been
-- filled in) then goes to memory to get the value.
tryCachedMemVal :: (Monad m) => MemVal m -> Maybe Val -> Either PhyReg Val -> Either PhyReg Val -> m (Maybe Val)
tryCachedMemVal memVal val base off =
    case val of
        Just v  -> return (Just v)
        Nothing -> either hasBase (return . Just) base where
            hasBase base      = either (hasOff base) (return . Just) off
            hasOff base' off' = fmap Just (memVal (fromIntegral $ base' + off'))

-- If the value to be loaded from memory has not yet been loaded, goes to memory.
-- Otherwise, no action is taken.
cachedMemVal :: (Monad m) => MemVal m -> Addr -> Maybe Val -> m (Maybe Val)
cachedMemVal memVal addr = maybe fetchVal (return . Just) where
    fetchVal = do
        val <- memVal addr
        return (Just val)

-- If the supplied Either already contains the value of the register, then
-- no action is taken. If the Either does not contain the value, the regVal
-- function is used to retrieve it.
cachedRegVal :: (Monad m) => RegVal m -> Either PhyReg Val -> m (Either PhyReg Val)
cachedRegVal regVal = either fetchVal (return . Right) where
    fetchVal r = do
        maybeVal <- regVal r
        return $ case maybeVal of
            Nothing  -> Left r
            Just val -> Right val
