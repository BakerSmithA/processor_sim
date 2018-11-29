module Pipeline where

import Instr
import WriteBack
import ROB (ROBIdx)

-- 5 stage pipeline: fetch, decod, execute, commit, and write-back.
data Pipeline = Pipeline {
    fetched   :: Maybe (ROBIdx, Instr)
  , decoded   :: Maybe (ROBIdx, Instr)
  , executed  :: Maybe (ROBIdx, WriteBack)
  , committed :: Maybe [WriteBack]
} deriving (Show)

-- Instruction that was fetched.
type Fetched a = (Maybe (ROBIdx, Instr), a)
-- Decodes a fetched instruction.
type Decoder m a = Instr -> a -> m (Instr, a)
-- Executes a decoded instruction.
type Executer m a = Instr -> a -> m (WriteBack, a)
-- Commits any executed instructions, and returns instructions that can be committed.
type Committer m a = (ROBIdx, WriteBack) -> a -> m ([WriteBack], a)
-- Writes instructions results to memory/registers.
type Writer m a = [WriteBack] -> a -> m a

-- Return pipeline with nothing in each stage.
empty :: Pipeline
empty = Pipeline Nothing Nothing Nothing Nothing

-- Helper function for steps of pipeline.
step :: (Monad m) => (a -> b -> m (a, c)) -> a -> Maybe b -> m (a, Maybe c)
step f x = maybe (return (x, Nothing)) success where
    success y = do
        (x', z) <- f x y
        return (x', Just z)

-- Steps a fetched instruction through the decode section of the pipeline.
decodeStep :: (Monad m) => Decoder m a -> a -> Maybe (ROBIdx, Instr) -> m (a, Maybe (ROBIdx, Instr))
decodeStep decode = step $ \x (idx, instr) -> do
    (decoded, x') <- decode instr x
    return (x', (idx, decoded))

-- Steps a decoded instruction through the exectution step of the pipeline.
execStep :: (Monad m) => Executer m a -> a -> Maybe (ROBIdx, Instr) -> m (a, Maybe (ROBIdx, WriteBack))
execStep exec = step $ \x (idx, instr) -> do
    (wb, x') <- exec instr x
    return (x', (idx, wb))

-- Steps an executed instruction through the commit stage of the pipeline.
commitStep :: (Monad m) => Committer m a -> a -> Maybe (ROBIdx, WriteBack) -> m (a, Maybe ([WriteBack]))
commitStep commit = step $ \x (idx, wb) -> do
    (wbs, x') <- commit (idx, wb) x
    return (x', wbs)

-- Steps a committed instruction through the writeback stage of the pipeline.
wbStep :: (Monad m) => Writer m a -> a -> Maybe [WriteBack] -> m a
wbStep write x = maybe (return x) (\wbs -> write wbs x)

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
    (x4, c) <- commitStep commit x3 (executed p)
    x5      <- wbStep     write  x4 (committed p)
    return (x5, Pipeline f d e c)
