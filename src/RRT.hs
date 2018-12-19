module RRT where

import Data.Map (Map)
import qualified Data.Map as Map
import Types

-- Pending mapping from architectural to physical register. Stored in ROB so
-- it can be flushed.
data RegMap
    = NoMap
    | RegMap RegIdx PhyReg
    deriving (Eq, Show)

-- Register Rename Table, holds a mapping from names of registers in source
-- code, e.g. reg 2, to physical registers, e.g. reg 45.
data RRT = RRT {
    reg2phy :: Map RegIdx PhyReg
  , phy2reg :: Map PhyReg RegIdx -- Required to make freeing O(1)
  , frees   :: [PhyReg]
} deriving (Show, Eq)

-- Return RRT with a maximum physical register index of that provided, useful
-- for testing.
empty :: PhyReg -> RRT
empty maxPhy = RRT Map.empty Map.empty [0..maxPhy]

-- Useful for testing.
fromMapping :: [(RegIdx, PhyReg)] -> [PhyReg] -> RRT
fromMapping reg2phy fs = RRT (Map.fromList reg2phy) (Map.fromList phy2reg) fs where
    phy2reg = fmap (\(reg, phy) -> (phy, reg)) reg2phy

-- Create RRT with registers mapped to physical registers.
-- Mapping is automatically decided. Useful for giving initial mapping to
-- special registers, e.g. PC.
fromRegs :: [RegIdx] -> PhyReg -> RRT
fromRegs rs maxPhy = foldr f (empty maxPhy) rs where
    f r rrt =
        case ins r rrt of
            Nothing -> error "No space to assign to initial registers"
            Just (_, rrt', _) -> rrt'

-- Create mapping from register name to physical register. The physical register
-- is chosen from the remaining free registers. Or, returns Nothing if there
-- are no free registers. Also returns the old register that was freed, if
-- one was freed.
ins :: RegIdx -> RRT -> Maybe (PhyReg, RRT, FreedReg)
ins _    (RRT _ _ []) = Nothing
ins name (RRT reg2phy phy2reg frees) = Just (phy, rrt', freedReg) where
    rrt'       = RRT reg2phy' phy2reg' rest'
    reg2phy'   = Map.insert name phy reg2phy
    phy2reg'   = Map.insert phy name phy2reg
    -- We are overwriting the register mapping. Therefore, if a mapping
    -- already exists it can be freed.
    rest'      = maybe rest (\r -> rest ++ [r]) freedReg
    freedReg   = Map.lookup name reg2phy
    (phy:rest) = frees

-- Insert mapping saved in ROB.
insMapping :: RegMap -> RRT -> RRT
insMapping NoMap rrt = rrt
insMapping (RegMap reg phy) rrt = rrt { reg2phy=reg2phy', phy2reg=phy2reg' } where
    reg2phy' = Map.insert reg phy (reg2phy rrt)
    phy2reg' = Map.insert phy reg (phy2reg rrt)

-- Makes a 'pending' assignment between an architectural and physical register.
-- The mapping is stored in the ROB.
assign :: RRT -> Maybe (PhyReg, RRT)
assign (RRT _ _ []) = Nothing
assign rrt = Just (phy, rrt') where
    rrt'       = rrt { frees=rest }
    (phy:rest) = (frees rrt)

free :: PhyReg -> RRT -> RRT
free phy rrt = rrt' where
    rrt'   = rrt { frees=frees' }
    frees' = (frees rrt) ++ [phy]

freeAll :: [PhyReg] -> RRT -> RRT
freeAll ps rrt = rrt { frees=frees' } where
    frees' = (frees rrt) ++ ps

-- Return physical register mapped to name of register in source code, or
-- Nothing if no mapping exists.
get :: RegIdx -> RRT -> Maybe PhyReg
get reg rrt = Map.lookup reg (reg2phy rrt)

-- Return where there is a mapping from the register to a physical register.
isPhy :: PhyReg -> RRT -> Bool
isPhy p rrt = p `Map.member` (phy2reg rrt)
