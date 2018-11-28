module ROB where

import Queue (Queue)
import qualified Queue as Q
import WriteBack

-- Entry into the Reorder Buffer. Whether the instruction is present determines
-- whether the entry is ready to be committed.
type Entry = Maybe WriteBack

-- Reorder Buffer, used to store write-back instructions before they are committed.
data ROB = ROB (Queue Entry)

-- Allocate a space for a not-yet-ready instruction, returning index to update
-- once the instruction is ready.
allocEmpty :: ROB -> (Int, ROB)
allocEmpty (ROB q) = (i, ROB q'') where
    (i, q') = Q.alloc q
    q''     = Q.set i Nothing q'

-- Return all instructions that can be committed, i.e. are ready and are at the
-- start of the queue.
commitable :: ROB -> ([WriteBack], ROB)
commitable (ROB q) = (wbs, ROB q') where
    (wbs, q') = commitable' q
    commitable' q =
        case Q.peek q of
            Nothing -> ([], q)
            Just wb -> (wb:wbs, q') where
                (wbs, q') = commitable' (Q.rem q)

-- Set the writeback instruction computed by the execution step.
set :: Int -> WriteBack -> ROB -> ROB
set i wb (ROB q) = ROB q' where
    q' = Q.set i (Just wb) q
