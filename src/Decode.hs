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

decodeI (MoveI r i) st1 = do
    (p, st2) <- takePReg r st1
    return (MoveI p i, st2)

decodeI (Move r from) st1 = do
    (pr, st2) <- takePReg r st1
    pfrom <- getPReg from st2
    return (Move pr pfrom, st2)

-- Convenience function for allocating a physical register.
takePReg :: RegIdx -> State -> MaybeT Res (PhyReg, State)
takePReg r st = MaybeT (return (St.allocPhyReg r st))

-- Convenience function for getting the mapping to a physical register.
getPReg :: RegIdx -> State -> MaybeT Res PhyReg
getPReg r st = MaybeT (fmap Just (St.getPhyReg r st))
