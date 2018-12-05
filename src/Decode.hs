module Decode where

import Instr
import State (State, Res)
import qualified State as St
import Control.Monad.Trans.State.Lazy (StateT(..), runStateT)

-- Because instruction are already parsed into struct, no need to decode.
-- However, register renaming will be performed at this step.
decode :: FInstr -> State -> Res (DInstr, State)
decode fi st = runStateT (decodeI fi) st

decodeI :: FInstr -> StateT State Res DInstr
decodeI = mapIM renameDst lookupSrc return

-- Takes a free physical register and renames the destination register.
renameDst :: RegIdx -> StateT State Res PhyReg
renameDst r = StateT (\st -> St.allocPhyReg r st)

-- Looks up the mapping from a source register to a physical register.
lookupSrc :: RegIdx -> StateT State Res PhyReg
lookupSrc r = StateT (\st -> fmap (\p -> (p, st)) (St.getPhyReg r st))
