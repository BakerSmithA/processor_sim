module ExecUnit where

-- Stores an instruction being performed.
data ExecUnit a = ExecUnit (Maybe a)

empty :: ExecUnit a
empty = ExecUnit Nothing

isFree :: ExecUnit a -> Bool
isFree (ExecUnit Nothing) = True
isFree _                  = False
