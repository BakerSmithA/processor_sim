module Instr where

import Data.Word (Word32)
import Reg (RegIdx)

type Addr = Word32
type Val = Word32

data Instr
    -- Memory
    = MoveI        { r :: RegIdx, val :: Val }                        -- r <- val
    | LoadIdx      { r :: RegIdx, base :: RegIdx, offset :: Addr }    -- r <- [[base] + offset]
    | LoadBaseIdx  { r :: RegIdx, base :: RegIdx, rOffset :: RegIdx } -- r <- [[base] + [R_offset]]
    | StoreIdx     { r :: RegIdx, base :: RegIdx, offset :: Addr }    -- r -> [[base] + offset]
    | StoreBaseIdx { r :: RegIdx, base :: RegIdx, rOffset :: RegIdx } -- r -> [[base] + [R_offset]]
    -- Arithmetic/Logic
    | Add  { r :: RegIdx, x :: RegIdx, y :: RegIdx } -- r <- [x] + [y]
    | AddI { r :: RegIdx, x :: RegIdx, i :: Val }    -- r <- [x] + i
    | Sub  { r :: RegIdx, x :: RegIdx, y :: RegIdx } -- r <- [x] - [y]
    | SubI { r :: RegIdx, x :: RegIdx, i :: Val }    -- r <- [x] - i
    | Eq   { r :: RegIdx, x :: RegIdx, y :: RegIdx } -- r <- [x] == [y]
    | EqI  { r :: RegIdx, x :: RegIdx, i :: Val }    -- r <- [x] == i
    -- Branching
    | B  { addr :: Addr }              -- Unconditional branch to addr
    | BT { r :: RegIdx, addr :: Addr } -- Branch to addr if r == 1
    | Ret                              -- Branch to address in link register.
    -- Debugging
    | Print { r :: RegIdx } -- Print value in a register.
    deriving (Eq, Show)
