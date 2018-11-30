module Decode where

import Instr
import State as St

-- Because instruction are already parsed into struct, no need to decode.
-- However, register renaming will be performed at this step.
-- decode :: FInstr -> State -> Res (Maybe DInstr, State)
-- decode i st = return (Just i, st)

-- Because instruction are already parsed into struct, no need to decode.
-- However, register renaming will be performed at this step.
decode :: FInstr -> State -> Res (Maybe DInstr, State)
decode fi st = return $ maybe (Nothing, st) just (decodeI fi st) where
    just (di, st') = (Just di, st')

decodeI :: FInstr -> State -> Maybe (DInstr, State)

decodeI (MoveI r v) st1 = do
    (p, st2) <- St.allocPhyReg r st1
    return (MoveI p v, st2)

decodeI (Move dst src) st1 = do
    (pdst, psrc, st2) <- St.alloc2PhyReg dst src st1
    return (Move pdst psrc, st2)
