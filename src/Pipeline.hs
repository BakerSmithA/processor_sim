module Pipeline where

import Instr

-- 4 stage pipeline, allowing for 4 instructions (at different stages) to be
-- processed at the same time.
data Pipeline = Pipeline {
    fetching  :: Maybe Instr
  , decoding  :: Maybe Instr
  , executing :: Maybe Instr
  , writing   :: Maybe Instr
}

-- Return pipeline with nothing in each stage.
empty :: Pipeline
empty = Pipeline Nothing Nothing Nothing Nothing

-- Generated by execution step of pipeline.
-- Instruction to machine of values to update.
data WriteBackInstr
    = WriteReg RegIdx Val
    | WriteMem Addr Val
    | WritePrint String
    | NoOp
