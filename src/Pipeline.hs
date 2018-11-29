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

-- Return pipeline with nothing in each stage.
empty :: Pipeline
empty = Pipeline Nothing Nothing Nothing Nothing

type Decoder   m   = Instr               -> m Instr
type Executer  m   = Instr               -> m WriteBack
type Committer m a = (ROBIdx, WriteBack) -> m ([WriteBack], a)
type Writer    m a = ([WriteBack], a)    -> m a

decodeFetched :: Maybe (ROBIdx, Instr) -> Decoder m -> m (Maybe (ROBIdx, Instr))
decodeFetched = undefined

execDecoded :: Maybe (ROBIdx, Instr) -> Executer m -> m (Maybe (ROBIdx, WriteBack))
execDecoded = undefined

commitExeced :: Maybe (ROBIdx, WriteBack) -> Committer m a -> m (Maybe ([WriteBack], a))
commitExeced = undefined

wbComitted :: Maybe ([WriteBack], a) -> Writer m a -> m (Maybe a)
wbComitted = undefined

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
    newDecoded  <- decodeFetched (fetched p) decode
    newExecuted <- execDecoded   (decoded p) exec
    newComitted <- commitExeced  (executed p) commit
    newWritten  <- wbComitted    newComitted write
    return (newWritten, Pipeline newFetched newDecoded newExecuted (fmap fst newComitted))

-- -- Helper for advance,
-- op :: (Monad m) => (a -> m b) -> Maybe a ->  m (Maybe b)
-- op f = maybe (return Nothing) (fmap Just . f)
--
-- -- Helper for advance which passes through the ROBIdx.
-- opRob :: (Monad m) => (a -> m b) -> Maybe (ROBIdx, a) ->  m (Maybe (ROBIdx, b))
-- opRob f = maybe failure success where
--     success (idx, x) = fmap (\x' -> Just (idx, x')) (f x)
--     failure          = return Nothing
--
-- -- Supplies new instruction into pipleine, and shifts in-flight instructions
-- -- through pipeline. Returns write-back result, and new state of pipeline.
-- advance :: (Monad m) => Maybe (ROBIdx, Instr)
--                      -> Decoder m
--                      -> Executer m
--                      -> Commiter m a
--                      -> Writer m a
--                      -> Pipeline
--                      -> m (Maybe a, Pipeline)
--
-- advance fetch decode exec commit write p = do
--     d <- opRob decode (fetched p)
--     e <- opRob exec (decoded p)
--     c <- op commit (executed p)
--     w <- write _
--     return (w, Pipeline fetch d e)
