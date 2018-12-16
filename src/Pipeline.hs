module Pipeline where

import Instr
import WriteBack
import Types

-- 5 stage pipeline: fetch, decod, execute, commit, and write-back.
data Pipeline = Pipeline {
    fetched   :: [FInstr]
  , decoded   :: [DPipeInstr]
  , executed  :: [(WriteBack, ROBIdx, FreedReg)]
} deriving (Show)

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

-- Helper function for steps of pipeline.
-- step :: (Monad m1, Monad m2) => m2 c -> (a -> b -> m1 (a, m2 c)) -> a -> [b] -> m1 (a, m2 c)
-- step empty f x = maybe (return (x, empty)) success where
--     success y = do
--         (x', z) <- f x y
--         return (x', z)

-- decodeStep decode x = foldM f ([], x) where
--     f (ds1, x1) fi = do
--         (ds2, x2) <- decode

--  ([DPipeInstr], a) -> FInstr -> m ([DPipeInstr], a)

-- decodeStep decode = step [] $ \x instr -> do
--     (decoded, x') <- decode instr x
--     return (x', decoded)

-- execStep exec = step [] $ \x instr -> do
--     (wbs, x') <- exec instr x
--     return (x', wbs)

-- Steps an executed instruction through the commit stage of the pipeline.
commitStep :: (Monad m) => Committer m a -> a -> [PipeData WriteBack] -> m a
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
    (d, x2) <- decode (fetched p) x1
    (e, x3) <- exec   (decoded p) x2 
    x4      <- commitStep commit  x3 (executed p)
    x5      <- write              x4
    return (x5, Pipeline f d e)
