module Pipeline where

import Instr
import WriteBack
import Types

-- 5 stage pipeline: fetch, decod, execute, commit, and write-back.
data Pipeline = Pipeline {
    fetched   :: Maybe (ROBIdx, FInstr)
  , decoded   :: Maybe (ROBIdx, DInstr)
  , executed  :: Maybe (ROBIdx, WriteBack)
} deriving (Show)

-- FInstruction that was fetched, or Nothing if stalled at this stage.
type Fetched a = (Maybe (ROBIdx, FInstr), a)
-- Decodes a fetched instruction, or Nothing if stalls at this stage.
type Decoder m a = FInstr -> a -> m (DInstr, a)
-- Executes a decoded instruction.
type Executer m a = DInstr -> a -> m (WriteBack, a)
-- Commits any executed instructions in ROB, and returns instructions that can be committed.
type Committer m a = (ROBIdx, WriteBack) -> a -> m a
-- Writes instructions results to memory/registers.
type Writer m a = a -> m a

-- Return pipeline with nothing in each stage.
empty :: Pipeline
empty = Pipeline Nothing Nothing Nothing

-- Helper function for steps of pipeline.
step :: (Monad m) => (a -> b -> m (a, Maybe c)) -> a -> Maybe b -> m (a, Maybe c)
step f x = maybe (return (x, Nothing)) success where
    success y = do
        (x', z) <- f x y
        return (x', z)

-- Steps a fetched instruction through the decode section of the pipeline.
decodeStep :: (Monad m) => Decoder m a -> a -> Maybe (ROBIdx, FInstr) -> m (a, Maybe (ROBIdx, DInstr))
decodeStep decode = step $ \x (idx, instr) -> do
    (decoded, x') <- decode instr x
    return (x', Just (idx, decoded))

-- Steps a decoded instruction through the exectution step of the pipeline.
execStep :: (Monad m) => Executer m a -> a -> Maybe (ROBIdx, DInstr) -> m (a, Maybe (ROBIdx, WriteBack))
execStep exec = step $ \x (idx, instr) -> do
    (wb, x') <- exec instr x
    return (x', Just (idx, wb))

-- Steps an executed instruction through the commit stage of the pipeline.
commitStep :: (Monad m) => Committer m a -> a -> Maybe (ROBIdx, WriteBack) -> m a
commitStep commit x = maybe (return x) (\wb -> commit wb x)

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
