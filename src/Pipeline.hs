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

-- Decodes a fetched instruction.
type Decoder m = Instr -> m Instr
-- Executes a decoded instruction.
type Executer m = Instr -> m WriteBack
-- Commits any executed instructions, and returns instructions that can be committed.
type Committer m a = (ROBIdx, WriteBack) -> m ([WriteBack], a)
-- Writes instructions results to memory/registers.
type Writer  m a = [WriteBack] -> a -> m a

-- Return pipeline with nothing in each stage.
empty :: Pipeline
empty = Pipeline Nothing Nothing Nothing Nothing

-- Performs a step of the pipeline.
step :: (Monad m) => (a -> m b) -> Maybe a -> m (Maybe b)
step f = maybe (return Nothing) success where
    success x = do
        y <- f x
        return (Just y)

-- Performs a step of the pipeline, passing through an acompanying ROBIdx.
stepPassROB :: (Monad m) => (a -> m b) -> Maybe (ROBIdx, a) -> m (Maybe (ROBIdx, b))
stepPassROB f = step $ \(idx, x) -> do
    y <- f x
    return (idx, y)

-- Supplies new instruction into pipleine, and shifts in-flight instructions
-- through pipeline. Returns write-back result, and new state of pipeline.
advance :: (Monad m) => Maybe (ROBIdx, Instr)
                     -> Decoder m
                     -> Executer m
                     -> Committer m a
                     -> Writer m a
                     -> Pipeline
                     -> m (Maybe a, Pipeline)

advance newFetched decode exec commit write p = do
    -- Decode the previously fetched instruction to obtain the currently decoded instruction.
    newDecoded  <- stepPassROB decode (fetched p)
    newExecuted <- stepPassROB exec   (decoded p)
    newComitted <- step        commit (executed p)
    newWritten  <- step        (uncurry write) newComitted
    return (newWritten, Pipeline newFetched newDecoded newExecuted (fmap fst newComitted))
