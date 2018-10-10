module Instr where

import Mem (Addr, Word32)
import Reg (RegIdx)

data Instr
    -- Memory
    = MoveI        { r :: RegIdx, val :: Word32 }                    -- r <- val
    | LoadIdx      { r :: RegIdx, base :: RegIdx, offset :: Addr }   -- r <- [base + offset]
    | LoadBaseIdx  { r :: RegIdx, base :: RegIdx, offset :: RegIdx } -- r <- [base + R_offset]
    | StoreIdx     { r :: RegIdx, base :: RegIdx, offset :: Addr }   -- r -> [base + offset]
    | StoreBaseIdx { r :: RegIdx, base :: RegIdx, offset :: RegIdx } -- r -> [base + R_offset]
    -- Arithmetic/Logic
    | Add  { r :: RegIdx, x :: RegIdx, y :: RegIdx } -- r <- x + y
    | AddI { r :: RegIdx, x :: RegIdx, i :: Word32 } -- r <- x + i
    | Sub  { r :: RegIdx, x :: RegIdx, y :: RegIdx } -- r <- x - y
    | SubI { r :: RegIdx, x :: RegIdx, i :: Word32 } -- r <- x - i
    -- Control
    | B   { addr :: Addr }              -- Unconditional branch to addr
    | BLT { r :: RegIdx, addr :: Addr } -- Branch to addr if r > 0
    | Ret                               -- Branch to address in link register.
