module Types where

import Data.Int (Int32)
import Data.Word (Word8, Word32)

type RegIdx = Word8
type PhyReg = Int
type Addr = Word32
type Val = Int32
type InstrAddr = Val
type ROBIdx = Int

-- Used to keep track of a register that was previously mapped from an
-- architectural register to a physical register. The value in the register
-- should be invalided at the writeback stage.
type FreedReg = Maybe PhyReg

-- Used for flushing pipeline.
type SavedPC = Val
