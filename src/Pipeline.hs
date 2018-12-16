module Pipeline where

import Instr
import WriteBack
import Types

-- 5 stage pipeline: fetch, decod, execute, commit, and write-back.
data Pipeline = Pipeline {
    fetched   :: [FInstr]
  , decoded   :: [DPipeInstr]
  , issued    :: [EPipeInstr]
  , executed  :: [(WriteBack, ROBIdx, FreedReg)]
} deriving (Show)

-- FInstruction that was fetched, or Nothing if stalled at this stage.
type Fetched a = ([FInstr], a)
-- Decodes a fetched instruction, or Nothing if stalls at this stage.
type Decoder m a = [FInstr] -> a -> m ([DPipeInstr], a)
-- Adds decoded instructions to the reorder buffer, and returns any instructions
-- that have all operands filled in.
type Issuer m a = [DPipeInstr] -> a -> m ([EPipeInstr], a)
-- Executes an instruction from a reservation station.
type Executer m a = [EPipeInstr] -> a -> m ([PipeData WriteBack], a)
-- Commits any executed instructions in ROB, and returns instructions that can be committed.
type Committer m a = [PipeData WriteBack] -> a -> m a
-- Writes instructions results to memory/registers.
type Writer m a = a -> m a

-- Return pipeline with nothing in each stage.
empty :: Pipeline
empty = Pipeline [] [] [] []

-- Steps an executed instruction through the commit stage of the pipeline.
commitStep :: (Monad m) => Committer m a -> a -> [PipeData WriteBack] -> m a
commitStep commit x wb = commit wb x

-- Supplies new instruction into pipleine, and shifts in-flight instructions
-- through pipeline. Returns write-back result, and new state of pipeline.
advance :: (Monad m) => Fetched a
                     -> Decoder m a
                     -> Issuer m a
                     -> Executer m a
                     -> Committer m a
                     -> Writer m a
                     -> Pipeline
                     -> m (a, Pipeline)

advance (f, x1) decode issue exec commit write p = do
    (d, x2) <- decode (fetched p) x1
    (i, x3) <- issue  (decoded p) x2
    (e, x4) <- exec   (issued  p) x3
    x5      <- commitStep commit  x4 (executed p)
    x6      <- write              x5
    return (x6, Pipeline f d i e)
