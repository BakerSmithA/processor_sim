module Pipeline where

import Instr
import WriteBack
import Types

-- Returned by write-back stage of pipeline to indicate whether pipeline should
-- be flushed.
data ShouldFlush
    = Flush
    | NoFlush

-- 5 stage pipeline: fetch, decode, execute, commit, and write-back.
-- The instructions stored in the pipeline represent instructions on wires
-- between stages.
data Pipeline = Pipeline {
    fetched   :: [FPipeInstr]
  , decoded   :: [DPipeInstr]
  , executed  :: [PipeData WriteBack]
}

instance Show Pipeline where
    show (Pipeline f d e)
        =  "Pipeline:\n"
        ++ "  F: " ++ (show f) ++ "\n"
        ++ "  D: " ++ (show d) ++ "\n"
        ++ "  E: " ++ (show e)

-- FInstruction that was fetched, or Nothing if stalled at this stage.
type Fetched a = ([FPipeInstr], a)
-- Decodes a fetched instruction, or Nothing if stalls at this stage.
type Decoder m a = [FPipeInstr] -> a -> m ([DPipeInstr], a)
-- Executes a decoded instruction.
type Executer m a = [DPipeInstr] -> a -> m ([PipeData WriteBack], a)
-- Commits any executed instructions in ROB, and returns instructions that can be committed.
type Committer m a = [PipeData WriteBack] -> a -> m a
-- Writes instructions results to memory/registers.
type Writer m a = a -> m (a, ShouldFlush, Maybe Addr)

type ShouldStall a = a -> Bool

-- Return pipeline with nothing in each stage.
empty :: Pipeline
empty = Pipeline [] [] []

-- Return pipeline with nothing in each stage.
flushed :: Pipeline
flushed = empty

-- Supplies new instruction into pipleine, and shifts in-flight instructions
-- through pipeline. Returns write-back result, and new state of pipeline.
advance :: (Monad m) => Fetched a
                     -> Decoder m a
                     -> ShouldStall a
                     -> Executer m a
                     -> Committer m a
                     -> Writer m a
                     -> Pipeline
                     -> m (a, Pipeline, ShouldFlush)

advance (f, x1) decode stallDecode exec commit write p = do
    -- If any of the stages below stall, then this also needs to stall to
    -- avoid instructions being overwritten.
    let f' = if stallDecode x1
                then (fetched p)
                else f

    (d, x2) <- if stallDecode x1
                    then decode [] x1
                    else decode (fetched p) x1

    (e, x3)                  <- exec (decoded p) x2
    x4                       <- commit (executed p) x3
    (x5, flush, invalidAddr) <- write x4

    case flush of
        NoFlush -> return (x5, Pipeline f' d (invalidateLoads invalidAddr e), NoFlush)
        Flush   -> return (x5, flushed, Flush)

stalled :: Bool -> [b] -> [b]
stalled True  _  = []
stalled False xs = xs

invalidateLoads :: Maybe Addr -> [PipeData WriteBack] -> [PipeData WriteBack]
invalidateLoads (Nothing)   wbs = wbs
invalidateLoads (Just addr) wbs = map (mapPipeData (invalidateLoad addr)) wbs
