module NewRS where

import Instr
import Types

type RegVal m = PhyReg -> m (Maybe Val)
type MemVal m = Addr   -> m Val

-- General pupose reservation station. Is specialised for different types of
-- instructions.
type RS a b = [Either a b]

run :: (Monad m) => (a -> m a) -> (a -> Maybe b) -> RS a b -> m ([b], RS a b)
run fill promote rs1 = do
    -- Do not modify any filled instructions that are still in the RS.
    rs2 <- mapM (either (fmap Left . fill) (return . Right)) rs1
    return (foldr checkDone ([], []) rs2) where
        checkDone i (execIs, rs) =
            case i of
                Right execInstr -> (execInstr:execIs, rs)
                Left  rsInstr   ->
                    case promote rsInstr of
                        Nothing        -> (execIs, i:rs)
                        Just execInstr -> (execInstr:execIs, rs)

-- Load store queue.
type LSQ = RS RSMemInstr EMemInstr

-- Tries to fill in operands of instructions in LSQ, and remove instructions
-- which can be run.
runLSQ :: (Monad m) => RegVal m -> MemVal m -> LSQ -> m ([EMemInstr], LSQ)
runLSQ regVal memVal = run (fillMem regVal memVal) promoteMem

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

-- If a memory instruction has all its operands filled, and possibly fetched
-- data from memory, then it is ready to be removed.
promoteMem :: RSMemInstr -> Maybe EMemInstr
promoteMem (LoadIdx (dst, val) base off) = do
    val'  <- val
    base' <- rightToMaybe base
    return (ELoad dst val' (fromIntegral $ base' + off))
promoteMem (LoadBaseIdx (dst, val) base off) = do
    val'  <- val
    base' <- rightToMaybe base
    off'  <- rightToMaybe off
    return (ELoad dst val' (fromIntegral $ base' + off'))
promoteMem (StoreIdx src base off) = do
    src'  <- rightToMaybe src
    base' <- rightToMaybe base
    return (EStore src' (fromIntegral $ base' + off))
promoteMem (StoreBaseIdx src base off) = do
    src'  <- rightToMaybe src
    base' <- rightToMaybe base
    off'  <- rightToMaybe off
    return (EStore src' (fromIntegral $ base' + off'))

type ArithLogicRS = RS RSALInstr EALInstr

-- Fills in operands of an AL instruction.
fillAL :: (Monad m) => RegVal m -> RSALInstr -> m RSALInstr
fillAL = mapALM return . fillRSrc

-- If the AL instruction has all operands filled in, then returns an executable
-- instruction.
promoteAL :: RSALInstr -> Maybe EALInstr
promoteAL = mapALM return f where
    f = either (const Nothing) Just

type BranchRS = RS RSBranchInstr EBranchInstr

-- Fills in operands of a branch instruction. For return instructions,
-- takes return address from link register.
fillB :: (Monad m) => RegVal m -> RSBranchInstr -> m RSBranchInstr
fillB regVal = mapBM (fillRSrc regVal) (fillRSrc regVal)

type OutRS = RS RSOutInstr EOutInstr

-- Fills in operands of an output instruction.
fillOut :: (Monad m) => RegVal m -> RSOutInstr -> m RSOutInstr
fillOut = mapOutM . fillRSrc

-- Helper functions.

-- If an operand register already has it value fetched, no action is taken.
-- Otherwise, the register file is queried.
fillRSrc :: (Monad m) => RegVal m -> Either PhyReg Val -> m (Either PhyReg Val)
fillRSrc regVal = either f (return . Right) where
    f phy = do
        maybeVal <- regVal phy
        return $ case maybeVal of
            Nothing  -> Left phy
            Just val -> Right val

rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just

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
