module Pipeline where

import Instr
import WriteBack

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
type Writer m a = a -> m (a, ShouldFlush)

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
                     -> ShouldStall a

                     -> Committer m a
                     -> ShouldStall a

                     -> Writer m a

                     -> Pipeline
                     -> m (a, Pipeline, ShouldFlush)

advance (f, x1) decode stallDecode exec stallExec commit stallCommit write p = do
    -- If any of the stages below stall, then this also needs to stall to
    -- avoid instructions being overwritten.
    let inDecode = stalled (stallDecode x1 || stallExec x1 || stallCommit x1) (fetched p)
    (d, x2) <- decode inDecode x1

    let inExec = stalled (stallExec x2 || stallCommit x2) (decoded p)
    (e, x3) <- exec inExec x2

    let inCommit = stalled (stallCommit x3) (executed p)
    x4          <- commit inCommit x3
    (x5, flush) <- write x4

    case flush of
        NoFlush -> return (x5, Pipeline f d e, NoFlush)
        Flush   -> return (x5, flushed, Flush)

stalled :: Bool -> [b] -> [b]
stalled True  _  = []
stalled False xs = xs
