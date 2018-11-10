module Bypass where

import Instr
import Pipeline
import Data.Map (Map)
import qualified Data.Map as Map

-- Data in the write-back stage that may be required for execution stage, e.g.
-- data dependencies:
--
--  ADD R0 R1 R2
--  SUB R3 R0 R4
--
-- There exists a data dependency because the value of R0 is computed by the ADD
-- and used in the SUB. Using a bypass, data is transferred backwards in the
-- piepline.
data Bypass = Bypass {
    -- Values to be written into registers.
    regVals :: Map RegIdx Val
    -- Values to be written into memory.
  , memVals :: Map Addr Val
}

empty :: Bypass
empty = Bypass Map.empty Map.empty

-- Return values of registers and memory that will be written back, to be made
-- available to execution stage of pipeline.
fromWriteback :: [WriteBackInstr] -> Bypass
fromWriteback = foldr addInstr Bypass.empty

-- Adds the result of a write-back instruction to the bypass values available to
-- the execution stage of the pipeline.
addInstr :: WriteBackInstr -> Bypass -> Bypass
addInstr (WriteReg reg val)  b = b { regVals = Map.insert reg val (regVals b) }
addInstr (WriteMem addr val) b = b { memVals = Map.insert addr val (memVals b) }
addInstr _ b = b

regVal :: RegIdx -> Bypass -> Maybe Val
regVal reg b = Map.lookup reg (regVals b)

memVal :: Addr -> Bypass -> Maybe Val
memVal addr b = Map.lookup addr (memVals b)
