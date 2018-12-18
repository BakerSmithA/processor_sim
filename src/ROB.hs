module ROB where

import Queue (Queue)
import qualified Queue as Q
import WriteBack
import Types

-- Entry into the Reorder Buffer. Whether the instruction is present determines
-- whether the entry is ready to be committed. Also stores the register to be
-- invalidated once writeback of the instruction occurs.
type Entry = Maybe (WriteBack, FreedReg, SavedPC)

mapEntry :: (WriteBack -> WriteBack) -> Entry -> Entry
mapEntry f = fmap (\(wb, freed, savedPC) -> (f wb, freed, savedPC))

-- Reorder Buffer, used to store write-back instructions before they are committed.
data ROB = ROB (Queue Entry)
         deriving (Eq)

instance Show ROB where
    show (ROB q) = show (Q.elemsNewOld q)

-- Creates a Reorder Buffer of the given length containing empty entries.
empty :: ROBIdx -> ROB
empty len = ROB (Q.fromList es) where
    es = replicate len Nothing

-- Removes all elements from ROB.
flush :: ROB -> ROB
flush (ROB q) = empty (Q.totalSize q)

-- Allocate a space for a not-yet-ready instruction, returning index to update
-- once the instruction is ready.
alloc :: ROB -> (ROBIdx, ROB)
alloc (ROB q) = (i, ROB q') where
    (i, q') = Q.enq Nothing q

-- Return all instructions that can be committed, i.e. are ready and are at the
-- start of the queue.
commitable :: ROB -> ([(WriteBack, FreedReg, SavedPC)], ROB)
commitable (ROB q) = (wbs, ROB q') where
    (wbs, q') = commitable' q
    commitable' q =
        case Q.peek q of
            Nothing -> ([], q)
            Just entry ->
                case entry of
                    Nothing -> ([], q)
                    Just wb -> (wb:wbs, q') where
                        (wbs, q') = commitable' (Q.rem q Nothing)

-- Set the writeback instruction computed by the execution step.
set :: ROBIdx -> WriteBack -> FreedReg -> SavedPC -> ROB -> ROB
set i wb freed savedPC (ROB q) = ROB q' where
    q' = Q.set i (Just (wb, freed, savedPC)) q

-- Searches in the ROB for physical register with matching register.
-- Nothing if an update to the register is not stored in the ROB.
regVal :: Q.Search -> PhyReg -> ROB -> Maybe Val
regVal search exp (ROB q) = do
    Just (WriteReg _ val _, _, _) <- Q.find search c q
    return val
        where
            c (Just (WriteReg reg _ _, _, _)) = reg == exp
            c _ = False

-- Searches in the ROB for matching memory address.
-- Or, returns Nothing if an update to the address is not stored in the ROB.
memVal :: Q.Search -> Addr -> ROB -> Maybe Val
memVal search exp (ROB q) = do
    Just (WriteMem _ val, _, _) <- Q.find search c q
    return val
        where
            c (Just (WriteMem addr _, _, _)) = addr == exp
            c _ = False

-- Searches through ROB, invalidating and loads which have the given address.
invalidateLoads :: Addr -> ROB -> ROB
invalidateLoads addr (ROB q) = ROB q' where
    q' = Q.mapQ (mapEntry (invalidateLoad addr)) q

-- Returns contents arranged newest to oldest. Useful for testing.
contents :: ROB -> [Entry]
contents (ROB q) = Q.elemsNewOld q

-- Returns all elements, including unassigned elements. Useful for testing.
allContents :: ROB -> [Entry]
allContents (ROB q) = Q.allElems q
