module Decode where

import Control.Monad.Trans.State.Lazy (StateT(..), runStateT)
import qualified Control.Monad.State as MSt
import Instr
import State (State, Res)
import qualified State as St
import Types

-- Because instruction are already parsed into struct, no need to decode.
-- However, register renaming will be performed at this step and allocting
-- the instruction a space in the reorder buffer.
decode :: FInstr -> State -> Res (DInstrIdx, State)
decode fi st = runStateT (decodeI fi) st

decodeI :: FInstr -> StateT State Res DInstrIdx
decodeI fi = do
    di <- mapIM renameDst renameDst lookupSrc fi
    let (di', freed) = MSt.runState (separateFreed di) Nothing
    robIdx <- allocROB
    return (di', robIdx, freed)

-- Takes freed register stored with destination register in instruction, and
-- writes it out, creating a DInstr. This separates the destination register
-- from the freed register.
separateFreed :: Instr (PhyReg, FreedReg) (PhyReg, FreedReg) PhyReg -> MSt.State FreedReg DInstr
separateFreed = mapIM f f return where
    f (phy, freed) = do
        MSt.put freed
        return phy

-- Takes a free physical register and renames the destination register.
-- Also returns the physical register that was freed, if the architectural
-- register was already assigned to a physical register. This allows
-- the value in this register to the invalidated at the write-back stage.
renameDst :: RegIdx -> StateT State Res (PhyReg, FreedReg)
renameDst r = StateT (\st -> St.allocPhyReg r st)

-- Looks up the mapping from a source register to a physical register.
lookupSrc :: RegIdx -> StateT State Res PhyReg
lookupSrc r = StateT (\st -> fmap (\p -> (p, st)) (St.getPhyReg r st))

-- Allocates a space in the reorder buffer.
allocROB :: StateT State Res ROBIdx
allocROB = StateT (\st -> return (St.allocROB st))
