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
allocEmpty (ROB q) = (i, ROB q') where
    (i, q') = Q.alloc q

-- Return all instructions that can be committed, i.e. are ready and are at the
-- start of the queue.
commit :: ROB -> ([WriteBack], ROB)
commit (ROB q) =
    case Q.peek q of
        Nothing -> ([], ROB q)
        Just wb -> (wb:wbs, rob) where
            (wbs, rob) = commit (ROB (Q.rem q))
