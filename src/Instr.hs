module Instr where

import Control.Monad.Identity
import Types

data MemInstr rDst rSrc
    = LoadIdx      rDst rSrc Val  -- r <- [[base] + offset]
    | LoadBaseIdx  rDst rSrc rSrc -- r <- [[base] + [R_offset]]
    | StoreIdx     rSrc rSrc Val  -- r -> [[base] + offset]
    | StoreBaseIdx rSrc rSrc rSrc -- r -> [[base] + [R_offset]]
    deriving (Eq, Show)

data ALInstr rDst rSrc
    = Add  rDst rSrc rSrc -- r <- [x] + [y]
    | AddI rDst rSrc Val  -- r <- [x] + i
    | Sub  rDst rSrc rSrc -- r <- [x] - [y]
    | SubI rDst rSrc Val  -- r <- [x] - i
    | Mult rDst rSrc rSrc -- r <- [x] * [y]
    | Div  rDst rSrc rSrc -- r <- [x] / [y]
    | Eq   rDst rSrc rSrc -- r <- [x] == [y]
    | Lt   rDst rSrc rSrc -- r <- [x] < [y]
    | Or   rDst rSrc rSrc -- r <- [x] || [y]
    | And  rDst rSrc rSrc -- r <- [x] && [y]
    | Not  rDst rSrc      -- r <- ![x]
    deriving (Eq, Show)

data BranchInstr rSrc
    = B  Addr      -- Unconditional branch to addr
    | BT rSrc Addr -- Branch to addr if r == 1
    | BF rSrc Addr -- Branch to addr if r == 0
    | Ret          -- Branch to address in link register.
    | SysCall      -- Terminates the program.
    deriving (Eq, Show)

data OutInstr rSrc
    = Print  rSrc -- Print value in a register.
    | PrintC rSrc -- Prints the value in a register as an ASCII character.
    | PrintLn     -- Print a newline.
    deriving (Eq, Show)

data Instr rDst rSrc
    = Mem (MemInstr rDst rSrc)
    | AL (ALInstr rDst rSrc)
    | Branch (BranchInstr rSrc)
    | Out (OutInstr rSrc)
    deriving (Eq, Show)

-- Fetched instruction.
type FInstr = Instr RegIdx RegIdx
-- Decoded instruction.
type DInstr = Instr PhyReg PhyReg
type DInstrIdx = (DInstr, ROBIdx, FreedReg)
-- Instruction stored in reservation station.
-- Stores partially 'filled-in' instrucions.
type RSInstrIdx = (Instr PhyReg (Either PhyReg Val), ROBIdx, FreedReg)
-- Executed instruction with computed values filled in.
type EInstr = Instr PhyReg Val
type EInstrIdx = (EInstr, ROBIdx, FreedReg)

-- = LoadIdx      { r    :: rDst, base :: rSrc, offset  :: Val }  -- r <- [[base] + offset]
-- | LoadBaseIdx  { r    :: rDst, base :: rSrc, rOffset :: rSrc } -- r <- [[base] + [R_offset]]
-- | StoreIdx     { rsrc :: rSrc, base :: rSrc, offset  :: Val }  -- r -> [[base] + offset]
-- | StoreBaseIdx { rsrc :: rSrc, base :: rSrc, rOffset :: rSrc } -- r -> [[base] + [R_offset]]

mapMem :: (d1 -> d2) -> (s1 -> s2) -> MemInstr d1 s1 -> MemInstr d2 s2
mapMem fd fs = runIdentity . mapMemM (return . fd) (return . fs)

mapMemM :: (Monad m) => (d1 -> m d2) -> (s1 -> m s2) -> MemInstr d1 s1 -> m (MemInstr d2 s2)
mapMemM fd fs (LoadIdx r b off) = do
    r' <- fd r
    b' <- fs b
    return (LoadIdx r' b' off)
mapMemM fd fs (LoadBaseIdx r b off) = do
    r' <- fd r
    b' <- fs b
    o' <- fs off
    return (LoadBaseIdx r' b' o')
mapMemM _ fs (StoreIdx r b off) = do
    r' <- fs r
    b' <- fs b
    return (StoreIdx r' b' off)
mapMemM _ fs (StoreBaseIdx r b off) = do
    r' <- fs r
    b' <- fs b
    o' <- fs off
    return (StoreBaseIdx r' b' o')

mapALM :: (Monad m) => (d1 -> m d2) -> (s1 -> m s2) -> ALInstr d1 s1 -> m (ALInstr d2 s2)
mapALM fd fs (Add  r x y) = mapDSS Add  (fd r) (fs x) (fs y)
mapALM fd fs (AddI r x i) = mapDSI AddI (fd r) (fs x) i
mapALM fd fs (Sub  r x y) = mapDSS Sub  (fd r) (fs x) (fs y)
mapALM fd fs (SubI r x i) = mapDSI SubI (fd r) (fs x) i
mapALM fd fs (Mult r x y) = mapDSS Mult (fd r) (fs x) (fs y)
mapALM fd fs (Div  r x y) = mapDSS Div  (fd r) (fs x) (fs y)
mapALM fd fs (Eq   r x y) = mapDSS Eq   (fd r) (fs x) (fs y)
mapALM fd fs (Lt   r x y) = mapDSS Lt   (fd r) (fs x) (fs y)
mapALM fd fs (Or   r x y) = mapDSS Or   (fd r) (fs x) (fs y)
mapALM fd fs (And  r x y) = mapDSS And  (fd r) (fs x) (fs y)
mapALM fd fs (Not  r x)   = do
    r' <- fd r
    x' <- fs x
    return (Not r' x')

mapDSS :: (Monad m) => (d2 -> s2 -> s2 -> ALInstr d2 s2)
                    -> m d2 -> m s2 -> m s2
                    -> m (ALInstr d2 s2)
mapDSS f dst src1 src2 = do
    dst' <- dst
    src1' <- src1
    src2' <- src2
    return (f dst' src1' src2')

mapDSI :: (Monad m) => (d2 -> s2 -> Val -> ALInstr d2 s2)
                    -> m d2 -> m s2 -> Val
                    -> m (ALInstr d2 s2)
mapDSI f dst src1 imm = do
    dst' <- dst
    src1' <- src1
    return (f dst' src1' imm)
