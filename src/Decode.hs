module Decode where

import Instr
import State

-- Because instruction are already parsed into struct, no need to decode.
-- However, register renaming will be performed at this step.
decode :: FInstr -> State -> Res (Maybe DInstr, State)
decode i st = return (Just i, st)
