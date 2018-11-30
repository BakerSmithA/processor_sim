module Decode where

import Instr
import State (State, Res)
import qualified State as St
import Control.Monad.Trans.Maybe

-- Because instruction are already parsed into struct, no need to decode.
-- However, register renaming will be performed at this step.
-- decode :: FInstr -> State -> Res (Maybe DInstr, State)
-- decode i st = return (Just i, st)

-- Because instruction are already parsed into struct, no need to decode.
-- However, register renaming will be performed at this step.
decode :: FInstr -> State -> Res (Maybe DInstr, State)
decode fi st = fmap (fmap addSt) runMaybeT (decodeI fi st) where
    addSt = maybe (Nothing, st) (\(di, st') -> (Just di, st'))

decodeI :: FInstr -> State -> MaybeT Res (DInstr, State)
decodeI (MoveI r i) = withPReg r $ \p _ ->
    return (MoveI p i)

decodeI (Move r from) = withPReg r $ \p st -> do
    pfrom <- getPReg from st
    return (Move p pfrom)

decodeI (LoadIdx r b off) = withPReg r $ \p st -> do
    pb <- getPReg b st
    return (LoadIdx p pb off)

decodeI (LoadBaseIdx r b off) = withPReg r $ \p st -> do
    pb   <- getPReg b st
    poff <- getPReg off st
    return (LoadBaseIdx p pb poff)

decodeI (StoreIdx r b off) = \st -> do
    pr   <- getPReg r st
    pb   <- getPReg b st
    return (StoreIdx pr pb off, st)

decodeI (StoreBaseIdx r b off) = \st -> do
    pr   <- getPReg r st
    pb   <- getPReg b st
    poff <- getPReg off st
    return (StoreBaseIdx pr pb poff, st)

decodeI (SysCall) = \st -> return (SysCall, st)

-- Takes a physical register and handles state.
withPReg :: RegIdx -> (PhyReg -> State -> MaybeT Res DInstr) -> State -> MaybeT Res (DInstr, State)
withPReg r f st1 = do
    (pr, st2) <- takePReg r st1
    fmap (\di -> (di, st2)) (f pr st2)

-- Convenience function for allocating a physical register.
takePReg :: RegIdx -> State -> MaybeT Res (PhyReg, State)
takePReg r st = MaybeT (return (St.allocPhyReg r st))

-- Convenience function for getting the mapping to a physical register.
getPReg :: RegIdx -> State -> MaybeT Res PhyReg
getPReg r st = MaybeT (fmap Just (St.getPhyReg r st))
