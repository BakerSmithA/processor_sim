module Decode where

import Control.Monad.Trans.State.Lazy (StateT(..), runStateT)
import Control.Monad (foldM)
import qualified Control.Monad.State as MSt
import Instr
import State (State, Res)
import qualified State as St
import Types

-- Because instruction are already parsed into struct, no need to decode.
-- However, register renaming will be performed at this step and allocting
-- the instruction a space in the reorder buffer.
decode :: [FPipeInstr] -> State -> Res ([DPipeInstr], State)
decode fis s = foldM f ([], s) fis where
    f (dis, st1) fi = do
        (di, st2) <- runStateT (decodeI fi) st1
        return (di:dis, st2)

decodeI :: FPipeInstr -> StateT State Res DPipeInstr
decodeI (fi, savedPC) = do
    robIdx <- allocROB
    di <- mapIM (renameDst robIdx) lookupSrc (const lookupLR) fi
    let (di', freed) = MSt.runState (separateFreed di) Nothing
    return (di', robIdx, freed, savedPC)

-- Takes freed register stored with destination register in instruction, and
-- writes it out, creating a DInstr. This separates the destination register
-- from the freed register.
separateFreed :: SameInstr (PhyReg, FreedReg) PhyReg PhyReg -> MSt.State FreedReg DInstr
separateFreed = mapIM f return return where
    f (phy, freed) = do
        MSt.put freed
        return phy

-- Takes a free physical register and renames the destination register.
-- Also returns the physical register that was freed, if the architectural
-- register was already assigned to a physical register. This allows
-- the value in this register to the invalidated at the write-back stage.
renameDst :: ROBIdx -> RegIdx -> StateT State Res (PhyReg, FreedReg)
renameDst robIdx r = StateT $ \st -> do
    -- Free the previous mapping.
    freedReg <- St.getMaybePhyReg r st
    (phy, st') <- St.allocPendingReg robIdx r st
    return ((phy, freedReg), st')

-- Looks up the mapping from a source register to a physical register.
lookupSrc :: RegIdx -> StateT State Res PhyReg
lookupSrc r = StateT (\st -> fmap (\p -> (p, st)) (St.getPhyReg r st))

-- Looks up the physical register mapping to the link register.
lookupLR :: StateT State Res PhyReg
lookupLR = StateT (\st -> fmap (\p -> (p, st)) (St.namedReg St.lrIdx st))

-- Allocates a space in the reorder buffer.
allocROB :: StateT State Res ROBIdx
allocROB = StateT (\st -> return (St.allocROB st))
