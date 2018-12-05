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
    fillRSrc :: Either PhyReg Val -> Res FillVal
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
-- Memory
rsToExec (MoveI        r i)     = Just (MoveI r i)
rsToExec (Move         r from)  = rsToExecRV  Move        r from
rsToExec (LoadIdx      r b off) = rsToExecRVV LoadIdx     r b (fillVal off)
rsToExec (LoadBaseIdx  r b off) = rsToExecRVV LoadBaseIdx r b off
rsToExec (StoreIdx     r b off) = rsToExecVVV StoreIdx    r b (fillVal off)
rsToExec (StoreBaseIdx r b off) = rsToExecVVV StoreIdx    r b off
-- Arithmetic/Logic
rsToExec (Add  r x y) = rsToExecRVV Add  r x y
rsToExec (AddI r x i) = rsToExecRVV AddI r x (fillVal i)
rsToExec (Sub  r x y) = rsToExecRVV Sub  r x y
rsToExec (SubI r x i) = rsToExecRVV SubI r x (fillVal i)
rsToExec (Mult r x y) = rsToExecRVV Mult r x y
rsToExec (Div  r x y) = rsToExecRVV Div  r x y
rsToExec (Eq   r x y) = rsToExecRVV Eq   r x y
rsToExec (Lt   r x y) = rsToExecRVV Lt   r x y
rsToExec (Or   r x y) = rsToExecRVV Or   r x y
rsToExec (And  r x y) = rsToExecRVV And  r x y
rsToExec (Not  r x)   = rsToExecRV  Not  r x
-- Branching
rsToExec (B    addr) = Just (B addr)
rsToExec (BT r addr) = rsToExecVA BT r addr
rsToExec (BF r addr) = rsToExecVA BF r addr
rsToExec (Ret)       = Just (Ret)
rsToExec (SysCall)   = Just (SysCall)
-- Debugging
rsToExec (Print  r) = rsToExecR Print  r
rsToExec (PrintC r) = rsToExecR PrintC r
rsToExec (PrintLn)  = Just (PrintLn)

-- Convenience function for checking RSInstr with a destination and source
-- register is full.
rsToExecRV :: (PhyReg -> Val -> EInstr) -> PhyReg -> FillVal -> Maybe EInstr
rsToExecRV f phy eVal = do
    val <- toMaybe eVal
    return (f phy val)

-- Convenience function for checking RSInstr with a destination and two source
-- registers is full.
rsToExecRVV :: (PhyReg -> Val -> Val -> EInstr) -> PhyReg -> FillVal -> FillVal -> Maybe EInstr
rsToExecRVV f phy ex ey = do
    x <- toMaybe ex
    y <- toMaybe ey
    return (f phy x y)

-- Convenience function for checking RSInstr with three source registers is full.
rsToExecVVV :: (Val -> Val -> Val -> EInstr) -> FillVal -> FillVal -> FillVal -> Maybe EInstr
rsToExecVVV f eSrc ex ey = do
    src <- toMaybe eSrc
    x <- toMaybe ex
    y <- toMaybe ey
    return (f src x y)

-- Convenience function for checking RSInstr with a source resgister and address
-- is full.
rsToExecVA :: (Val -> Addr -> EInstr) -> FillVal -> Addr -> Maybe EInstr
rsToExecVA f er addr = do
    r <- toMaybe er
    return (f r addr)

-- Convenience function for checking RSInstr with a single source register is full.
rsToExecR :: (Val -> EInstr) -> FillVal -> Maybe EInstr
rsToExecR f er = do
    r <- toMaybe er
    return (f r)

fillVal :: Val -> FillVal
fillVal = Right

-- Returns Right value or either, or Nothing if Left.
toMaybe :: Either a b -> Maybe b
toMaybe = either (const Nothing) Just
