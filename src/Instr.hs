module Instr where

import Data.Int (Int32)
import Data.Word (Word8, Word32)

type RegIdx = Word8
type PhyReg = Int
type Addr = Word32
type Val = Int32

data TemplateInstr rDst rSrc addr
    -- Memory
    = MoveI        { r    :: rDst, val :: Val }                    -- r <- val
    | Move         { r    :: rDst, from :: rSrc }                  -- r <- [from]
    | LoadIdx      { r    :: rDst, base :: rSrc, offset  :: Val }  -- r <- [[base] + offset]
    | LoadBaseIdx  { r    :: rDst, base :: rSrc, rOffset :: rSrc } -- r <- [[base] + [R_offset]]
    | StoreIdx     { rsrc :: rSrc, base :: rSrc, offset  :: Val }  -- r -> [[base] + offset]
    | StoreBaseIdx { rsrc :: rSrc, base :: rSrc, rOffset :: rSrc } -- r -> [[base] + [R_offset]]
    -- Arithmetic/Logic
    | Add  { r :: rDst, x :: rSrc, y :: rSrc } -- r <- [x] + [y]
    | AddI { r :: rDst, x :: rSrc, i :: Val }  -- r <- [x] + i
    | Sub  { r :: rDst, x :: rSrc, y :: rSrc } -- r <- [x] - [y]
    | SubI { r :: rDst, x :: rSrc, i :: Val }  -- r <- [x] - i
    | Mult { r :: rDst, x :: rSrc, y :: rSrc } -- r <- [x] * [y]
    | Div  { r :: rDst, x :: rSrc, y :: rSrc } -- r <- [x] / [y]
    | Eq   { r :: rDst, x :: rSrc, y :: rSrc } -- r <- [x] == [y]
    | Lt   { r :: rDst, x :: rSrc, y :: rSrc } -- r <- [x] < [y]
    | Or   { r :: rDst, x :: rSrc, y :: rSrc } -- r <- [x] || [y]
    | And  { r :: rDst, x :: rSrc, y :: rSrc } -- r <- [x] && [y]
    | Not  { r :: rDst, x :: rSrc }            -- r <- ![x]
    -- Branching
    | B  { addr :: addr }               -- Unconditional branch to addr
    | BT { rsrc :: rSrc, addr :: addr } -- Branch to addr if r == 1
    | BF { rsrc :: rSrc, addr :: addr } -- Branch to addr if r == 0
    | Ret                               -- Branch to address in link register.
    | SysCall                           -- Terminates the program.
    -- Debugging
    | Print  { rsrc :: rSrc } -- Print value in a register.
    | PrintC { rsrc :: rSrc } -- Prints the value in a register as an ASCII character.
    | PrintLn                 -- Print a newline.
    deriving (Eq, Show)

-- Fetched instruction.
type FInstr = TemplateInstr RegIdx RegIdx Addr
-- Decoded instruction.
type DInstr = TemplateInstr PhyReg PhyReg Addr
-- Instruction stored in reservation station.
-- Stores partially 'filled-in' instrucions.
type RSInstr = TemplateInstr PhyReg (Either PhyReg Val) (Either Addr Val)
-- Executed instruction with computed values filled in.
type EInstr = TemplateInstr PhyReg Val Val

-- Map register and address values stored in instruction.
mapI :: (rDst1 -> rDst2) -> (rSrc1 -> rSrc2) -> (a1 -> a2) -> TemplateInstr rDst1 rSrc1 a1 -> TemplateInstr rDst2 rSrc2 a2
-- Memory
mapI fd _  _ (MoveI r v)            = MoveI (fd r) v
mapI fd fs _ (Move r from)          = Move (fd r) (fs from)
mapI fd fs _ (LoadIdx r b off)      = LoadIdx (fd r) (fs b) off
mapI fd fs _ (LoadBaseIdx r b off)  = LoadBaseIdx (fd r) (fs b) (fs off)
mapI _  fs _ (StoreIdx r b off)     = StoreIdx (fs r) (fs b) off
mapI _  fs _ (StoreBaseIdx r b off) = StoreBaseIdx (fs r) (fs b) (fs off)
-- Arithmetic/Logic
mapI fd fs _ (Add  r x y) = Add  (fd r) (fs x) (fs y)
mapI fd fs _ (AddI r x i) = AddI (fd r) (fs x) i
mapI fd fs _ (Sub  r x y) = Sub  (fd r) (fs x) (fs y)
mapI fd fs _ (SubI r x i) = SubI (fd r) (fs x) i
mapI fd fs _ (Mult r x y) = Mult (fd r) (fs x) (fs y)
mapI fd fs _ (Div  r x y) = Div  (fd r) (fs x) (fs y)
mapI fd fs _ (Eq   r x y) = Eq   (fd r) (fs x) (fs y)
mapI fd fs _ (Lt   r x y) = Lt   (fd r) (fs x) (fs y)
mapI fd fs _ (Or   r x y) = Or   (fd r) (fs x) (fs y)
mapI fd fs _ (And  r x y) = And  (fd r) (fs x) (fs y)
mapI fd fs _ (Not  r x)   = Not  (fd r) (fs x)
-- Branching
mapI _  _ fa (B addr)    = B  (fa addr)
mapI _ fs fa (BT r addr) = BT (fs r) (fa addr)
mapI _ fs fa (BF r addr) = BF (fs r) (fa addr)
mapI _ _  _  (Ret)       = Ret
mapI _ _  _  (SysCall)   = SysCall
-- Debugging
mapI _ fs _ (Print  r) = Print  (fs r)
mapI _ fs _ (PrintC r) = PrintC (fs r)
mapI _ _  _ (PrintLn)  = PrintLn

isBranch :: TemplateInstr rDst rSrc a -> Bool
isBranch (B _)    = True
isBranch (BT _ _) = True
isBranch (BF _ _) = True
isBranch (Ret)    = True
isBranch _        = False
