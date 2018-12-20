module ExecUnit where

import Instr

-- Stores an instruction being performed.
data ExecUnit a = ExecUnit (Maybe a)
                deriving (Eq, Show)

type MemUnit = ExecUnit (PipeData EMemInstr)
type ALUnit  = ExecUnit (PipeData EALInstr)
type BUnit   = ExecUnit (PipeData EBranchInstr)
type OutUnit = ExecUnit (PipeData EOutInstr)

empty :: ExecUnit a
empty = ExecUnit Nothing

isFree :: ExecUnit a -> Bool
isFree (ExecUnit Nothing) = True
isFree _                  = False
