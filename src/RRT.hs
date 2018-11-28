module RRT where

import Data.Map (Map)
import qualified Data.Map as Map
import Instr (RegIdx)

-- Physical register index.
type PhyReg = RegIdx

-- Register Rename Table, holds a mapping from names of registers in source
-- code, e.g. reg 2, to physical registers, e.g. reg 45.
data RRT = RRT { mapping :: Map RegIdx PhyReg, frees :: [PhyReg] }

-- Create mapping from register name to physical register. The physical register
-- is chosen from the remaining free registers. Or, returns Nothing if there
-- are no free registers.
ins :: RegIdx -> RRT -> Maybe (PhyReg, RRT)
ins _    (RRT _ [])         = Nothing
ins name (RRT m (phy:rest)) = Just (phy, RRT (Map.insert name phy m) rest)

-- Frees the name of a register from being mapped to a physical register, and
-- make the physical register available to others.
free :: PhyReg -> RRT -> RRT
free phy rrt = rrt { mapping = mapping', frees = frees' } where
    mapping' = Map.delete phy (mapping rrt)
    frees'   = phy:(frees rrt)
