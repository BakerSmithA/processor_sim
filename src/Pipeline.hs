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
type Executer m a = Instr -> a -> m WriteBack
-- Commits any executed instructions, and returns instructions that can be committed.
type Committer m a = (ROBIdx, WriteBack) -> a -> m ([WriteBack], a)
-- Writes instructions results to memory/registers.
type Writer  m a = [WriteBack] -> a -> m a

-- Return pipeline with nothing in each stage.
empty :: Pipeline
empty = Pipeline Nothing Nothing Nothing Nothing

-- Steps a fetched instruction through the decode section of the pipeline.
decodeStep :: (Monad m) => Decoder m a -> Maybe (ROBIdx, Instr) -> a -> m (a, Maybe (ROBIdx, Instr))
decodeStep = undefined

-- decodeStep decode x = maybe (return (Nothing, x)) success where
--     success (idx, instr) = do
--         (instr', x') <- decode instr x
--         return $ Just (idx, instr', x')

execStep :: (Monad m) => Executer m a -> Maybe (ROBIdx, Instr) -> a -> m (a, Maybe (ROBIdx, WriteBack))
execStep = undefined

-- Supplies new instruction into pipleine, and shifts in-flight instructions
-- through pipeline. Returns write-back result, and new state of pipeline.
advance :: (Monad m) => Fetched a
                     -> Decoder m a
                     -> Executer m a
                     -> Committer m a
                     -> Writer m a
                     -> Pipeline
                     -> m (Maybe a, Pipeline)

advance newFetched decode exec commit write p = do
    undefined

snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x

-- -- Performs a step of the pipeline.
-- step :: (Monad m) => (a -> m b) -> Maybe a -> m (Maybe b)
-- step f = maybe (return Nothing) success where
--     success x = do
--         y <- f x
--         return (Just y)
--
-- -- Performs a step of the pipeline, passing through an acompanying ROBIdx.
-- stepPassROB :: (Monad m) => (a -> m b) -> Maybe (ROBIdx, a) -> m (Maybe (ROBIdx, b))
-- stepPassROB f = step $ \(idx, x) -> do
--     y <- f x
--     return (idx, y)

-- advance newFetched decode exec commit write p = do
--     -- Decode the previously fetched instruction to obtain the currently decoded instruction.
--     newDecoded  <- stepPassROB decode (fetched p)
--     newExecuted <- stepPassROB exec   (decoded p)
--     newComitted <- step        commit (executed p)
--     newWritten  <- step        (uncurry write) newComitted
--     return (newWritten, Pipeline newFetched newDecoded newExecuted (fmap fst newComitted))
