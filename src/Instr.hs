module Instr where

import Data.Int (Int32)
import Data.Word (Word8, Word32)

type RegIdx = Word8
type PhyReg = Int
type Addr = Word32
type Val = Int32

data TemplateInstr reg addr
    -- Memory
    = MoveI        { r :: reg, val :: Val }                  -- r <- val
    | Move         { r :: reg, from :: reg }                 -- r <- [from]
    | LoadIdx      { r :: reg, base :: reg, offset :: Val }  -- r <- [[base] + offset]
    | LoadBaseIdx  { r :: reg, base :: reg, rOffset :: reg } -- r <- [[base] + [R_offset]]
    | StoreIdx     { r :: reg, base :: reg, offset :: Val }  -- r -> [[base] + offset]
    | StoreBaseIdx { r :: reg, base :: reg, rOffset :: reg } -- r -> [[base] + [R_offset]]
    -- Arithmetic/Logic
    | Add  { r :: reg, x :: reg, y :: reg } -- r <- [x] + [y]
    | AddI { r :: reg, x :: reg, i :: Val } -- r <- [x] + i
    | Sub  { r :: reg, x :: reg, y :: reg } -- r <- [x] - [y]
    | SubI { r :: reg, x :: reg, i :: Val } -- r <- [x] - i
    | Mult { r :: reg, x :: reg, y :: reg } -- r <- [x] * [y]
    | Div  { r :: reg, x :: reg, y :: reg } -- r <- [x] / [y]
    | Eq   { r :: reg, x :: reg, y :: reg } -- r <- [x] == [y]
    | Lt   { r :: reg, x :: reg, y :: reg } -- r <- [x] < [y]
    | Or   { r :: reg, x :: reg, y :: reg } -- r <- [x] || [y]
    | And  { r :: reg, x :: reg, y :: reg } -- r <- [x] && [y]
    | Not  { r :: reg, x :: reg }           -- r <- ![x]
    -- Branching
    | B  { addr :: addr }              -- Unconditional branch to addr
    | BT { r    :: reg, addr :: addr } -- Branch to addr if r == 1
    | BF { r    :: reg, addr :: addr } -- Branch to addr if r == 0
    | Ret                              -- Branch to address in link register.
    | SysCall                          -- Terminates the program.
    -- Debugging
    | Print { r :: reg }  -- Print value in a register.
    | PrintC { r :: reg } -- Prints the value in a register as an ASCII character.
    | PrintLn             -- Print a newline.
    deriving (Eq, Show)

-- Fetched instruction.
type FInstr = TemplateInstr RegIdx Addr
-- Decoded instruction.
type DInstr = TemplateInstr PhyReg Addr

isBranch :: TemplateInstr r a -> Bool
isBranch (B _)    = True
isBranch (BT _ _) = True
isBranch (BF _ _) = True
isBranch (Ret)    = True
isBranch _        = False
