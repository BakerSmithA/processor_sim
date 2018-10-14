module Instr where

import Data.Word (Word32)
import Reg (RegIdx)

type Addr = Word32
type Val = Word32

data Instr
    -- Memory
    = MoveI        { r :: RegIdx, val :: Val }                        -- r <- val
    | Move         { r :: RegIdx, from :: RegIdx }                    -- r <- [from]
    | LoadIdx      { r :: RegIdx, base :: RegIdx, offset :: Addr }    -- r <- [[base] + offset]
    | LoadBaseIdx  { r :: RegIdx, base :: RegIdx, rOffset :: RegIdx } -- r <- [[base] + [R_offset]]
    | StoreIdx     { r :: RegIdx, base :: RegIdx, offset :: Addr }    -- r -> [[base] + offset]
    | StoreBaseIdx { r :: RegIdx, base :: RegIdx, rOffset :: RegIdx } -- r -> [[base] + [R_offset]]
    -- Arithmetic/Logic
    | Add  { r :: RegIdx, x :: RegIdx, y :: RegIdx } -- r <- [x] + [y]
    | Sub  { r :: RegIdx, x :: RegIdx, y :: RegIdx } -- r <- [x] - [y]
    | Eq   { r :: RegIdx, x :: RegIdx, y :: RegIdx } -- r <- [x] == [y]
    | Or   { r :: RegIdx, x :: RegIdx, y :: RegIdx } -- r <- [x] || [y]
    | And  { r :: RegIdx, x :: RegIdx, y :: RegIdx } -- r <- [x] && [y]
    -- Branching
    | B  { addr :: Addr }              -- Unconditional branch to addr
    | BT { r :: RegIdx, addr :: Addr } -- Branch to addr if r == 1
    | Ret                              -- Branch to address in link register.
    -- Debugging
    | Print { r :: RegIdx } -- Print value in a register.
    deriving (Eq, Show)
