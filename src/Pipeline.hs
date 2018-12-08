module Pipeline where

import Instr
import WriteBack
import Types

-- 5 stage pipeline: fetch, decod, execute, commit, and write-back.
data Pipeline = Pipeline {
    fetched   :: Maybe FInstr
  , decoded   :: Maybe DPipeInstr
  , executed  :: [(WriteBack, ROBIdx, FreedReg)]
} deriving (Show)

-- FInstruction that was fetched, or Nothing if stalled at this stage.
type Fetched a = (Maybe FInstr, a)
-- Decodes a fetched instruction, or Nothing if stalls at this stage.
type Decoder m a = FInstr -> a -> m (DPipeInstr, a)
-- Executes a decoded instruction.
type Executer m a = DPipeInstr -> a -> m ([(WriteBack, ROBIdx, FreedReg)], a)
-- Commits any executed instructions in ROB, and returns instructions that can be committed.
type Committer m a = [(WriteBack, ROBIdx, FreedReg)] -> a -> m a
-- Writes instructions results to memory/registers.
type Writer m a = a -> m a

-- Return pipeline with nothing in each stage.
empty :: Pipeline
empty = Pipeline Nothing Nothing []

-- Helper function for steps of pipeline.
step :: (Monad m1, Monad m2) => m2 c -> (a -> b -> m1 (a, m2 c)) -> a -> Maybe b -> m1 (a, m2 c)
step empty f x = maybe (return (x, empty)) success where
    success y = do
        (x', z) <- f x y
        return (x', z)

-- Steps a fetched instruction through the decode section of the pipeline.
decodeStep :: (Monad m) => Decoder m a -> a -> Maybe FInstr -> m (a, Maybe DPipeInstr)
decodeStep decode = step Nothing $ \x instr -> do
    (decoded, x') <- decode instr x
    return (x', Just decoded)

-- Steps a decoded instruction through the exectution step of the pipeline.
execStep :: (Monad m) => Executer m a -> a -> Maybe DPipeInstr -> m (a, [(WriteBack, ROBIdx, FreedReg)])
execStep exec = step [] $ \x instr -> do
    (wbs, x') <- exec instr x
    return (x', wbs)

-- Steps an executed instruction through the commit stage of the pipeline.
commitStep :: (Monad m) => Committer m a -> a -> [(WriteBack, ROBIdx, FreedReg)] -> m a
commitStep commit x wb = commit wb x

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
    (x2, d) <- decodeStep decode x1 (fetched p)
    (x3, e) <- execStep   exec   x2 (decoded p)
    x4      <- commitStep commit x3 (executed p)
    x5      <- write             x4
    return (x5, Pipeline f d e)
