module RS where

import Instr
import Types

-- Need to supply ROB index so the logically most recent update to a value
-- can be retrieved.
type RegVal m = ROBIdx -> PhyReg -> m (Maybe Val)
type MemVal m = ROBIdx -> Addr -> m Val

-- General pupose reservation station. Is specialised for different types of
-- instructions.
type RS a = [PipeData a]

empty :: RS a
empty = []

fromList :: [PipeData a] -> RS a
fromList = id

add :: PipeData a -> RS a -> RS a
add = (:)

isEmpty :: RS a -> Bool
isEmpty = null

-- Fills in operands in an instruction.
fillOperands :: (Monad m) => (ROBIdx -> a -> m a) -> RS a -> m (RS a)
fillOperands fill = mapM f where
    f = mapPipeDataM' (\idx i -> fill idx i)

-- Removes the oldest instruction found in the RS which has all operands filled.
-- Returns RS without any removed instructions.
promote :: (a -> Maybe b) -> RS a -> (Maybe (PipeData b), RS a)
promote checkDone = foldr f (Nothing, []) where
    f entry (Just i,  rs) = (Just i, entry:rs)
    f entry (Nothing, rs) =
        case mapPipeDataM checkDone entry of
            Nothing -> (Nothing, entry:rs)
            Just ei -> (Just ei, rs)

-- Load store queue.
type MemRS = RS RSMemInstr

-- Prepare a decoded instruction to be placed in the MemRS.
rsMemInstr :: DMemInstr -> RSMemInstr
rsMemInstr = mapMem (\r -> (r, Nothing)) Left

-- Fills in operands of any memory instructions in the RS.
fillMemRS :: (Monad m) => RegVal m -> MemVal m -> MemRS -> m MemRS
fillMemRS regVal memVal = fillOperands (fillMem regVal memVal)

-- Promotes the oldest instruction in the RS with all instructions filled.
promoteMemRS :: MemRS -> (Maybe (PipeData EMemInstr), MemRS)
promoteMemRS = promote promoteMem

-- Fills in operands of a single memory instruction.
-- Goes to memory to load a value for load instructions.
fillMem :: (Functor m, Monad m) => RegVal m -> MemVal m -> ROBIdx -> RSMemInstr -> m RSMemInstr
fillMem regVal memVal robIdx = fill where
    fill (LoadIdx (dst, val) base off) = do
        base' <- cachedRegVal regVal robIdx base
        val'  <- tryCachedMemVal memVal robIdx val base' (return off)
        return (LoadIdx (dst, val') base' off)

    fill (LoadBaseIdx (dst, val) base off) = do
        base' <- cachedRegVal regVal robIdx base
        off'  <- cachedRegVal regVal robIdx off
        val'  <- tryCachedMemVal memVal robIdx val base' off'
        return (LoadBaseIdx (dst, val') base' off')

    fill (StoreIdx src base off) = do
        src'  <- cachedRegVal regVal robIdx src
        base' <- cachedRegVal regVal robIdx base
        return (StoreIdx src' base' off)

    fill (StoreBaseIdx src base off) = do
        src'  <- cachedRegVal regVal robIdx src
        base' <- cachedRegVal regVal robIdx base
        off'  <- cachedRegVal regVal robIdx off
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

type ArithLogicRS = RS RSALInstr

-- Prepare a decoded instruction to be placed in the RS.
rsALInstr :: DALInstr -> RSALInstr
rsALInstr = mapAL id Left

-- Fills in operands of any AL instructions in the RS.
fillALRS :: (Monad m) => RegVal m -> ArithLogicRS -> m ArithLogicRS
fillALRS regVal = fillOperands (fillAL regVal)

-- Promotes the oldest instruction in the RS with all instructions filled.
promoteALRS :: ArithLogicRS -> (Maybe (PipeData EALInstr), ArithLogicRS)
promoteALRS = promote promoteAL

-- Fills in operands of an AL instruction.
fillAL :: (Functor m, Monad m) => RegVal m -> ROBIdx -> RSALInstr -> m RSALInstr
fillAL regVal robIdx = mapALM return (fillRSrc regVal robIdx)

-- If the AL instruction has all operands filled in, then returns an executable
-- instruction.
promoteAL :: RSALInstr -> Maybe EALInstr
promoteAL = mapALM return f where
    f = either (const Nothing) Just

type BranchRS = RS RSBranchInstr

-- Prepare a decoded instruction to be placed in the RS.
rsBInstr :: DBranchInstr -> RSBranchInstr
rsBInstr = mapB Left Left

-- Fills in operands of any branch instructions in the RS.
fillBRS :: (Monad m) => RegVal m -> BranchRS -> m BranchRS
fillBRS regVal = fillOperands (fillB regVal)

-- Promotes the oldest instruction in the RS with all instructions filled.
promoteBRS :: BranchRS -> (Maybe (PipeData EBranchInstr), BranchRS)
promoteBRS = promote promoteB

-- Fills in operands of a branch instruction. For return instructions,
-- takes return address from link register.
fillB :: (Functor m, Monad m) => RegVal m -> ROBIdx -> RSBranchInstr -> m RSBranchInstr
fillB regVal robIdx = mapBM (fillRSrc regVal robIdx) (fillRSrc regVal robIdx)

-- If the branch instruction has all operands filled in, then returns an
-- executable instruction.
promoteB :: RSBranchInstr -> Maybe EBranchInstr
promoteB = mapBM f f where
    f = either (const Nothing) Just

type OutRS = RS RSOutInstr

-- Prepare a decoded instruction to be placed in the RS.
rsOutInstr :: DOutInstr -> RSOutInstr
rsOutInstr = mapOut Left

-- Fills in operands of any output instructions in the RS.
fillOutRS :: (Monad m) => RegVal m -> OutRS -> m OutRS
fillOutRS regVal = fillOperands (fillOut regVal)

-- Promotes the oldest instruction in the RS with all instructions filled.
promoteOutRS :: OutRS -> (Maybe (PipeData EOutInstr), OutRS)
promoteOutRS = promote promoteOut

-- Fills in operands of an output instruction.
fillOut :: (Functor m, Monad m) => RegVal m -> ROBIdx -> RSOutInstr -> m RSOutInstr
fillOut regVal robIdx = mapOutM (fillRSrc regVal robIdx)

-- If an output instruction has all operands filled in, then returns an
-- executable instruction.
promoteOut :: RSOutInstr -> Maybe EOutInstr
promoteOut = mapOutM f where
    f = either (const Nothing) Just

-- Helper functions.

-- If an operand register already has it value fetched, no action is taken.
-- Otherwise, the register file is queried.
fillRSrc :: (Functor m, Monad m) => RegVal m -> ROBIdx -> Either PhyReg Val -> m (Either PhyReg Val)
fillRSrc regVal robIdx = either f (return . Right) where
    f phy = do
        maybeVal <- regVal robIdx phy
        return $ case maybeVal of
            Nothing  -> Left phy
            Just val -> Right val

rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just

-- If the value of an address has already been fetched, no action is performed.
-- Otherwise, if the address can be calculated (because both operands have been
-- filled in) then goes to memory to get the value.
tryCachedMemVal :: (Functor m, Monad m) => MemVal m -> ROBIdx -> Maybe Val -> Either PhyReg Val -> Either PhyReg Val -> m (Maybe Val)
tryCachedMemVal memVal robIdx val base off =
    case val of
        Just v  -> return (Just v)
        Nothing ->
            case base of
                Left _      -> return Nothing
                Right base' ->
                    case off of
                        Left _     -> return Nothing
                        Right off' -> fmap Just (memVal robIdx addr) where
                            addr = fromIntegral $ base' + off'

-- If the supplied Either already contains the value of the register, then
-- no action is taken. If the Either does not contain the value, the regVal
-- function is used to retrieve it.
cachedRegVal :: (Functor m, Monad m) => RegVal m -> ROBIdx -> Either PhyReg Val -> m (Either PhyReg Val)
cachedRegVal regVal robIdx = either fetchVal (return . Right) where
    fetchVal r = do
        maybeVal <- regVal robIdx r
        return $ case maybeVal of
            Nothing  -> Left r
            Just val -> Right val
