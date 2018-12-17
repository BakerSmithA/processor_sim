module Pipeline where

import Instr
import WriteBack
import Types

-- 5 stage pipeline: fetch, decod, execute, commit, and write-back.
data Pipeline = Pipeline {
    fetched   :: [FInstr]
  , decoded   :: [DPipeInstr]
  , executed  :: [(WriteBack, ROBIdx, FreedReg)]
}

instance Show Pipeline where
    show (Pipeline f d e)
        =  "Pipeline:\n"
        ++ "  F: " ++ (show f) ++ "\n"
        ++ "  D: " ++ (show d) ++ "\n"
        ++ "  E: " ++ (show e)

-- FInstruction that was fetched, or Nothing if stalled at this stage.
type Fetched a = ([FInstr], a)
-- Decodes a fetched instruction, or Nothing if stalls at this stage.
type Decoder m a = [FInstr] -> a -> m ([DPipeInstr], a)
-- Executes a decoded instruction.
type Executer m a = [DPipeInstr] -> a -> m ([PipeData WriteBack], a)
-- Commits any executed instructions in ROB, and returns instructions that can be committed.
type Committer m a = [PipeData WriteBack] -> a -> m a
-- Writes instructions results to memory/registers.
type Writer m a = a -> m a

-- Return pipeline with nothing in each stage.
empty :: Pipeline
empty = Pipeline [] [] []

-- Supplies new instruction into pipleine, and shifts in-flight instructions
-- through pipeline. Returns write-back result, and new state of pipeline.
advance :: (Monad m) => Fetched a
                     -> Decoder m a
                     -> Executer m a
                     -> Committer m a
                     -> Writer m a
                     -> Pipeline
                     -> m (a, Pipeline)

advance (f, x1) decode exec commit write p = do
    (d, x2) <- decode (fetched p) x1
    (e, x3) <- exec   (decoded p) x2
    x4      <- commit (executed p) x3
    x5      <- write              x4
    return (x5, Pipeline f d e)
