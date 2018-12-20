module ExecUnit where

import Instr
import WriteBack

-- Stores an instruction being performed.
data ExecUnit a = ExecUnit (Maybe a)
                deriving (Eq, Show)

instance Functor ExecUnit where
    -- (a -> b) -> ExecUnit a -> ExecUnit b
    fmap f (ExecUnit x) = ExecUnit (fmap f x)

type MemUnit = ExecUnit (PipeData WriteBack)
type ALUnit  = ExecUnit (PipeData WriteBack)
type BUnit   = ExecUnit (PipeData WriteBack)
type OutUnit = ExecUnit (PipeData WriteBack)

value :: ExecUnit a -> Maybe a
value (ExecUnit x) = x

empty :: ExecUnit a
empty = ExecUnit Nothing

containing :: a -> ExecUnit a
containing = ExecUnit . Just

isFree :: ExecUnit a -> Bool
isFree (ExecUnit Nothing) = True
isFree _                  = False
