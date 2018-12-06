module Decode where

import Control.Monad.Trans.State.Lazy (StateT(..), runStateT)
import Instr
import State (State, Res)
import qualified State as St
import Types

-- Because instruction are already parsed into struct, no need to decode.
-- However, register renaming will be performed at this step and allocting
-- the instruction a space in the reorder buffer.
decode :: FInstr -> State -> Res ((ROBIdx, DInstr), State)
decode fi st = runStateT (decodeI fi) st

decodeI :: FInstr -> StateT State Res (ROBIdx, DInstr)
decodeI fi = do
    di <- mapIM renameDst lookupSrc return fi
    robIdx <- allocROB
    return (robIdx, di)

-- Takes a free physical register and renames the destination register.
renameDst :: RegIdx -> StateT State Res PhyReg
renameDst r = StateT (\st -> St.allocPhyReg r st)

-- Looks up the mapping from a source register to a physical register.
lookupSrc :: RegIdx -> StateT State Res PhyReg
lookupSrc r = StateT (\st -> fmap (\p -> (p, st)) (St.getPhyReg r st))

-- Allocates a space in the reorder buffer.
allocROB :: StateT State Res ROBIdx
allocROB = StateT (\st -> return (St.allocROB st))
