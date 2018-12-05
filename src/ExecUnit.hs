module ExecUnit where

import Instr
import WriteBack

-- Load/Store Unit.
lsu :: EInstr -> WriteBack
lsu = undefined

-- Arithmetic/Logic Unit.
alu :: EInstr -> WriteBack
alu = undefined

-- Branch Unit.
bu :: EInstr -> WriteBack
bu = undefined

-- Output Unit (for debugging).
ou :: EInstr -> WriteBack
ou = undefined
