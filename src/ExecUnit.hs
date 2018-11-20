module ExecUnit where

import Instr
import RS

type Occupied = Bool

-- Used to represent ALU, Load/Store Unit, etc
data ExecUnit = ExecUnit ((Instr, FilledOp) -> a) Occupied
