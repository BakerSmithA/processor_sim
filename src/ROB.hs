module ROB where

import Queue (Queue)
import qualified Queue as Q
import WriteBack
import Instr

type ROBIdx = Int

-- Entry into the Reorder Buffer. Whether the instruction is present determines
-- whether the entry is ready to be committed.
type Entry = Maybe WriteBack

-- Reorder Buffer, used to store write-back instructions before they are committed.
data ROB = ROB (Queue Entry)
         deriving (Eq, Show)

-- Creates a Reorder Buffer of the given length containing empty entries.
empty :: ROBIdx -> ROB
empty len = ROB (Q.fromList es) where
    es = replicate len Nothing

-- Allocate a space for a not-yet-ready instruction, returning index to update
-- once the instruction is ready.
alloc :: ROB -> (ROBIdx, ROB)
alloc (ROB q) = (i, ROB q') where
    (i, q') = Q.enq Nothing q

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
set :: ROBIdx -> WriteBack -> ROB -> ROB
set i wb (ROB q) = ROB q' where
    q' = Q.set i (Just wb) q

-- Searches in the ROB for the most recent value of a register, or returns
-- Nothing if an update to the register is not stored in the ROB.
regVal :: RegIdx -> ROB -> Maybe Val
regVal exp (ROB q) = do
    Just (WriteReg _ val) <- Q.findNewest c q
    return val
        where
            c (Just (WriteReg reg _)) = reg == exp
            c _ = False

-- Searches in the ROB for the most recent value of a memory address, or
-- returns Nothing if an update to the address is not stored in the ROB.
memVal :: Addr -> ROB -> Maybe Val
memVal exp (ROB q) = do
    Just (WriteMem _ val) <- Q.findNewest c q
    return val
        where
            c (Just (WriteMem addr _)) = addr == exp
            c _ = False
