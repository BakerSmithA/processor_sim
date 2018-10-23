module Instr where

import Data.Int (Int32)
import Data.Word (Word8, Word32)

type RegIdx = Word8
type Addr = Word32
type Val = Int32

data Instr
    -- Memory
    = MoveI        { r :: RegIdx, val :: Val }                        -- r <- val
    | Move         { r :: RegIdx, from :: RegIdx }                    -- r <- [from]
    | LoadIdx      { r :: RegIdx, base :: RegIdx, offset :: Val }    -- r <- [[base] + offset]
    | LoadBaseIdx  { r :: RegIdx, base :: RegIdx, rOffset :: RegIdx } -- r <- [[base] + [R_offset]]
    | StoreIdx     { r :: RegIdx, base :: RegIdx, offset :: Val }    -- r -> [[base] + offset]
    | StoreBaseIdx { r :: RegIdx, base :: RegIdx, rOffset :: RegIdx } -- r -> [[base] + [R_offset]]
    -- Arithmetic/Logic
    | Add  { r :: RegIdx, x :: RegIdx, y :: RegIdx } -- r <- [x] + [y]
    | AddI { r :: RegIdx, x :: RegIdx, i :: Val }    -- r <- [x] + i
    | Sub  { r :: RegIdx, x :: RegIdx, y :: RegIdx } -- r <- [x] - [y]
    | SubI { r :: RegIdx, x :: RegIdx, i :: Val }    -- r <- [x] - i
    | Mult { r :: RegIdx, x :: RegIdx, y :: RegIdx } -- r <- [x] * [y]
    | Eq   { r :: RegIdx, x :: RegIdx, y :: RegIdx } -- r <- [x] == [y]
    | Lt   { r :: RegIdx, x :: RegIdx, y :: RegIdx } -- r <- [x] < [y]
    | Or   { r :: RegIdx, x :: RegIdx, y :: RegIdx } -- r <- [x] || [y]
    | And  { r :: RegIdx, x :: RegIdx, y :: RegIdx } -- r <- [x] && [y]
    | Not  { r :: RegIdx, x :: RegIdx }              -- r <- ![x]
    -- Branching
    | B  { addr :: Addr }              -- Unconditional branch to addr
    | BT { r :: RegIdx, addr :: Addr } -- Branch to addr if r == 1
    | Ret                              -- Branch to address in link register.
    | SysCall                          -- Terminates the program.
    -- Debugging
    | Print { r :: RegIdx } -- Print value in a register.
    | PrintLn               -- Print a newline.
    deriving (Eq, Show)
