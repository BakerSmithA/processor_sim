module ROB where

import Queue (Queue)
import qualified Queue as Q
import WriteBack
import RRT (RegMap(..))
import Types

-- Entry into the Reorder Buffer. Whether the instruction is present determines
-- whether the entry is ready to be committed. Also stores the register to be
-- invalidated once writeback of the instruction occurs.
type Entry = (Maybe (WriteBack, FreedReg, SavedPC), RegMap)

emptyEntry :: Entry
emptyEntry = (Nothing, NoMap)

mapEntry :: (WriteBack -> WriteBack) -> Entry -> Entry
mapEntry f (x, regMap) = (fmap modify x, regMap) where
    modify (wb, freed, savedPC) = (f wb, freed, savedPC)

-- Reorder Buffer, used to store write-back instructions before they are committed.
data ROB = ROB (Queue Entry)
         deriving (Eq)

instance Show ROB where
    show (ROB q) = unlines (fmap (\s -> "\t" ++ show s) (Q.elemsNewOld q))

-- Creates a Reorder Buffer of the given length containing empty entries.
empty :: ROBIdx -> ROB
empty len = ROB (Q.fromList es) where
    es = replicate len emptyEntry

-- Return number of empty spaces in ROB.
freeSpace :: ROB -> Int
freeSpace (ROB q) = Q.freeSpace q

-- Removes all elements from ROB.
flush :: ROB -> ROB
flush (ROB q) = empty (Q.totalSize q)

-- Allocate a space for a not-yet-ready instruction, returning index to update
-- once the instruction is ready.
alloc :: ROB -> (ROBIdx, ROB)
alloc (ROB q) = (i, ROB q') where
    (i, q') = Q.enq emptyEntry q

-- Return all instructions that can be committed, i.e. are ready and are at the
-- start of the queue, up to the oldest invalid load (if one exists).
commitable :: ROB -> ([(WriteBack, FreedReg, RegMap)], Maybe SavedPC, ROB)
commitable (ROB q) = (wbs, savedPC, ROB q') where
    (wbs, savedPC, q') = commitable' q
    commitable' q =
        case Q.peek q of
            Nothing -> ([], Nothing, q)
            Just (entry, regMap) ->
                case entry of
                    Nothing -> ([], Nothing, q)
                    Just (wb, freed, pc) ->
                        if isInvalidLoad wb
                            then ([], Just pc, q)
                            else ((wb,freed,regMap):wbs, savedPC, q') where
                                (wbs, savedPC, q') = commitable' (Q.rem q emptyEntry)

-- Stores a mapping from an architectural to physical register in the ROB.
-- This allows mappings to be reverted if a flush occurs.
setRegMap :: ROBIdx -> RegIdx -> PhyReg -> ROB -> ROB
setRegMap i reg phy (ROB q) = ROB q' where
    q' = Q.set i (entry, RegMap reg phy) q
    (entry, _) = Q.get i q

-- Set the writeback instruction computed by the execution step.
set :: ROBIdx -> WriteBack -> FreedReg -> SavedPC -> ROB -> ROB
set i wb freed savedPC (ROB q) = ROB q' where
    q' = Q.set i (Just (wb, freed, savedPC), regMap) q
    (_, regMap) = Q.get i q

-- Return instruction next to be removed from ROB.
peek :: ROB -> Maybe (WriteBack, FreedReg, SavedPC)
peek (ROB q) = do
    (e, _) <- Q.peek q
    e

-- Searches in the ROB for physical register with matching register.
-- Nothing if an update to the register is not stored in the ROB.
regVal :: Q.Search -> PhyReg -> ROB -> Maybe Val
regVal search exp (ROB q) = do
    (Just (WriteReg _ val _, _, _), _) <- Q.find search c q
    return val
        where
            c (Just (WriteReg reg _ _, _, _), _) = reg == exp
            c _ = False

-- Searches in the ROB for matching memory address.
-- Or, returns Nothing if an update to the address is not stored in the ROB.
memVal :: Q.Search -> Addr -> ROB -> Maybe Val
memVal search exp (ROB q) = do
    (Just (WriteMem _ val, _, _), _) <- Q.find search c q
    return val
        where
            c (Just (WriteMem addr _, _, _), _) = addr == exp
            c _ = False

-- Return a 'pending' mapping from an architectural register to a physical register.
regMap :: RegIdx -> ROB -> Maybe PhyReg
regMap reg (ROB q) = do
    (_, RegMap _ phy) <- Q.find Q.NewToOld matches q
    return phy
        where
            matches (_, RegMap r _) = r == reg
            matches _               = False

-- Searches through ROB, invalidating and loads which have the given address.
invalidateLoads :: Addr -> ROB -> ROB
invalidateLoads addr (ROB q) = ROB q' where
    q' = Q.mapQ (mapEntry (invalidateLoad addr)) q

-- Returns all mappings to physical registers stored in the ROB.
mappedPhyRegs :: ROB -> [PhyReg]
mappedPhyRegs (ROB q) = foldr f [] (Q.elemsNewOld q) where
    f (_, RegMap _ phy) acc = phy:acc
    f _ acc = acc

-- Returns contents arranged newest to oldest. Useful for testing.
contents :: ROB -> [Entry]
contents (ROB q) = Q.elemsNewOld q

-- Returns all elements, including unassigned elements. Useful for testing.
allContents :: ROB -> [Entry]
allContents (ROB q) = Q.allElems q
