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
-- Instruction stored in reservation station.
-- Stores partially 'filled-in' instrucions.
type RSInstr = TemplateInstr (Either PhyReg Val) (Either Addr Val)

-- Map register and address values stored in instruction.
mapI :: (r1 -> r2) -> (a1 -> a2) -> TemplateInstr r1 a1 -> TemplateInstr r2 a2
-- Memory.
mapI fr _ (MoveI r v)            = MoveI        (fr r) v
mapI fr _ (Move r from)          = Move         (fr r) (fr from)
mapI fr _ (LoadIdx r b off)      = LoadIdx      (fr r) (fr b) off
mapI fr _ (LoadBaseIdx r b off)  = LoadBaseIdx  (fr r) (fr b) (fr off)
mapI fr _ (StoreIdx r b off)     = StoreIdx     (fr r) (fr b) off
mapI fr _ (StoreBaseIdx r b off) = StoreBaseIdx (fr r) (fr b) (fr off)
-- Arithmetic/Logic.
mapI fr _ (Add  r x y) = Add  (fr r) (fr x) (fr y)
mapI fr _ (AddI r x i) = AddI (fr r) (fr x) i
mapI fr _ (Sub  r x y) = Sub  (fr r) (fr x) (fr y)
mapI fr _ (SubI r x i) = SubI (fr r) (fr x) i
mapI fr _ (Mult r x y) = Mult (fr r) (fr x) (fr y)
mapI fr _ (Div  r x y) = Div  (fr r) (fr x) (fr y)
mapI fr _ (Eq   r x y) = Eq   (fr r) (fr x) (fr y)
mapI fr _ (Lt   r x y) = Lt   (fr r) (fr x) (fr y)
mapI fr _ (Or   r x y) = Or   (fr r) (fr x) (fr y)
mapI fr _ (And  r x y) = And  (fr r) (fr x) (fr y)
mapI fr _ (Not  r x)   = Not  (fr r) (fr x)
-- Branching
mapI _  fa (B addr)    = B (fa addr)
mapI fr fa (BT r addr) = BT (fr r) (fa addr)
mapI fr fa (BF r addr) = BF (fr r) (fa addr)
mapI _  _  (Ret)       = Ret
mapI _  _  (SysCall)   = SysCall
-- Debugging
mapI fr _ (Print r)  = Print (fr r)
mapI fr _ (PrintC r) = PrintC (fr r)
mapI _  _ (PrintLn)  = PrintLn

isBranch :: TemplateInstr r a -> Bool
isBranch (B _)    = True
isBranch (BT _ _) = True
isBranch (BF _ _) = True
isBranch (Ret)    = True
isBranch _        = False
