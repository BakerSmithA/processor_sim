module RRT where

import Data.Map (Map)
import qualified Data.Map as Map
import Instr (RegIdx)
import Control.Applicative ((<|>))

-- Physical register index.
type PhyReg = RegIdx

-- Register Rename Table, holds a mapping from names of registers in source
-- code, e.g. reg 2, to physical registers, e.g. reg 45.
data RRT = RRT {
    reg2phy :: Map RegIdx PhyReg
  , phy2reg :: Map PhyReg RegIdx -- Required to make freeing O(1)
  , consts  :: Map RegIdx PhyReg -- For named registers, e.g. pc, sp, lr, etc
  , frees   :: [PhyReg]
} deriving (Show, Eq)

-- Return RRT with a maximum physical register index of that provided, useful
-- for testing.
empty :: PhyReg -> RRT
empty maxPhy = RRT Map.empty Map.empty Map.empty [0..maxPhy]

-- Useful for testing.
fromMapping :: [(RegIdx, PhyReg)] -> [(RegIdx, PhyReg)] -> [PhyReg] -> RRT
fromMapping reg2phy consts fs = RRT (Map.fromList reg2phy) (Map.fromList phy2reg) (Map.fromList consts) fs where
    phy2reg = fmap (\(reg, phy) -> (phy, reg)) reg2phy

-- Create RRT with specific registers, e.g. pc, already mapped.
fromConstRegs :: [RegIdx] -> PhyReg -> RRT
fromConstRegs regs maxPhy = fromMapping [] consts fs where
    consts = [(reg, maxPhy-i) | (reg, i) <- zip regs (reverse [0..(fromIntegral $ length regs - 1)])]
    fs = [0..(maxPhy-(fromIntegral $ length regs))]

-- Create mapping from register name to physical register. The physical register
-- is chosen from the remaining free registers. Or, returns Nothing if there
-- are no free registers.
ins :: RegIdx -> RRT -> Maybe (PhyReg, RRT)
ins _    (RRT _ _ _ [])                      = Nothing
ins name (RRT reg2phy phy2reg cs (phy:rest)) = Just (phy, rrt') where
    rrt'     = RRT reg2phy' phy2reg' cs rest
    reg2phy' = Map.insert name phy reg2phy
    phy2reg' = Map.insert phy name phy2reg

-- Frees the physical register from being mapped to a register name.
-- Returns Nothing if there is no mapping to the physical register.
free :: PhyReg -> RRT -> Maybe RRT
free phy rrt = do
    reg <- Map.lookup phy (phy2reg rrt)
    let reg2phy' = Map.delete reg (reg2phy rrt)
        phy2reg' = Map.delete phy (phy2reg rrt)
        frees'   = phy:(frees rrt)
    return rrt { reg2phy=reg2phy', phy2reg=phy2reg', frees=frees' }

-- Return physical register mapped to name of register in source code, or
-- Nothing if no mapping exists.
get :: RegIdx -> RRT -> Maybe PhyReg
get reg rrt = Map.lookup reg (consts rrt)
          <|> Map.lookup reg (reg2phy rrt)
