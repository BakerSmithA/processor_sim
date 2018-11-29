module RRT where

import Data.Map (Map)
import qualified Data.Map as Map
import Instr (RegIdx)

-- Physical register index.
type PhyReg = RegIdx

-- Register Rename Table, holds a mapping from names of registers in source
-- code, e.g. reg 2, to physical registers, e.g. reg 45.
data RRT = RRT { mapping :: Map RegIdx PhyReg, frees :: [PhyReg] }
         deriving (Show, Eq)

-- Return RRT with a maximum physical register index of that provided.
empty :: RegIdx -> RegIdx -> RegIdx -> RegIdx -> RegIdx -> PhyReg -> RRT
empty pc sp lr bp ret maxPhy = RRT (Map.empty) [0..maxPhy]

-- Useful for testing.
fromMapping :: [(RegIdx, PhyReg)] -> [PhyReg] -> RRT
fromMapping m fs = RRT (Map.fromList m) fs

-- Create mapping from register name to physical register. The physical register
-- is chosen from the remaining free registers. Or, returns Nothing if there
-- are no free registers.
ins :: RegIdx -> RRT -> Maybe RRT
ins _    (RRT _ [])         = Nothing
ins name (RRT m (phy:rest)) = Just (RRT (Map.insert name phy m) rest)

-- Frees the name of a register from being mapped to a physical register, and
-- make the physical register available to others. Returns Nothing if there
-- is no mapping from the given register name.
free :: RegIdx -> RRT -> Maybe RRT
free reg rrt = do
    phy <- get reg rrt
    let mapping' = Map.delete reg (mapping rrt)
        frees'   = phy:(frees rrt)
    return rrt { mapping = mapping', frees = frees' }

-- Return physical register mapped to name of register in source code, or
-- Nothing if no mapping exists.
get :: RegIdx -> RRT -> Maybe PhyReg
get reg rrt = Map.lookup reg (mapping rrt)
