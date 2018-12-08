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
    = MoveI rDst Val       -- r <- i
    | Move  rDst rSrc      -- r <- [x]
    | Add   rDst rSrc rSrc -- r <- [x] + [y]
    | AddI  rDst rSrc Val  -- r <- [x] + i
    | Sub   rDst rSrc rSrc -- r <- [x] - [y]
    | SubI  rDst rSrc Val  -- r <- [x] - i
    | Mult  rDst rSrc rSrc -- r <- [x] * [y]
    | Div   rDst rSrc rSrc -- r <- [x] / [y]
    | Eq    rDst rSrc rSrc -- r <- [x] == [y]
    | Lt    rDst rSrc rSrc -- r <- [x] < [y]
    | Or    rDst rSrc rSrc -- r <- [x] || [y]
    | And   rDst rSrc rSrc -- r <- [x] && [y]
    | Not   rDst rSrc      -- r <- ![x]
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

data Instr rDst memRDst rSrc
    = Mem (MemInstr memRDst rSrc)
    | AL (ALInstr rDst rSrc)
    | Branch (BranchInstr rSrc)
    | Out (OutInstr rSrc)
    deriving (Eq, Show)

-- Fetched instruction.
type FInstr = Instr RegIdx RegIdx RegIdx
-- Decoded instruction.
type DInstr = Instr PhyReg PhyReg PhyReg
type DInstrIdx = (DInstr, ROBIdx, FreedReg)
-- Instruction stored in reservation station.
-- Stores partially 'filled-in' instrucions.
type RSInstrIdx = (Instr PhyReg PhyReg (Either PhyReg Val), ROBIdx, FreedReg)
-- Executed instruction with computed values filled in.
type EInstr = Instr PhyReg PhyReg Val
type EInstrIdx = (EInstr, ROBIdx, FreedReg)

type EMemInstr    = MemInstr PhyReg Val
type EALInstr     = ALInstr PhyReg Val
type EBranchInstr = BranchInstr Val
type EOutInstr    = OutInstr Val

loadIdx :: l -> s -> Val -> Instr d l s
loadIdx r base off = Mem (LoadIdx r base off)

loadBaseIdx :: l -> s -> s -> Instr d l s
loadBaseIdx r base off = Mem (LoadBaseIdx r base off)

storeIdx :: s -> s -> Val -> Instr d l s
storeIdx r base off = Mem (StoreIdx r base off)

storeBaseIdx :: s -> s -> s -> Instr d l s
storeBaseIdx r base off = Mem (StoreBaseIdx r base off)

moveI :: d -> Val -> Instr d l s
moveI r i = AL (MoveI r i)

move :: d -> s -> Instr d l s
move r x = AL (Move r x)

add :: d -> s -> s -> Instr d l s
add r x y = AL (Add r x y)

addI :: d -> s -> Val -> Instr d l s
addI r x i = AL (AddI r x i)

sub :: d -> s -> s -> Instr d l s
sub r x y = AL (Sub r x y)

subI :: d -> s -> Val -> Instr d l s
subI r x i = AL (SubI r x i)

mult :: d -> s -> s -> Instr d l s
mult r x y = AL (Mult r x y)

divI :: d -> s -> s -> Instr d l s
divI r x y = AL (Div r x y)

eq :: d -> s -> s -> Instr d l s
eq r x y = AL (Eq r x y)

lt :: d -> s -> s -> Instr d l s
lt r x y = AL (Lt r x y)

orI :: d -> s -> s -> Instr d l s
orI r x y = AL (Or r x y)

andI :: d -> s -> s -> Instr d l s
andI r x y = AL (And r x y)

notI :: d -> s -> Instr d l s
notI r x = AL (Not r x)

b :: Addr -> Instr d l s
b addr = Branch (B addr)

bt :: s -> Addr -> Instr d l s
bt r addr = Branch (BT r addr)

bf :: s -> Addr -> Instr d l s
bf r addr = Branch (BF r addr)

ret :: Instr d l s
ret = Branch Ret

sysCall :: Instr d l s
sysCall = Branch SysCall

printI :: s -> Instr d l s
printI r = Out (Print r)

printC :: s -> Instr d l s
printC r = Out (PrintC r)

printLn :: Instr d l s
printLn = Out (PrintLn)

isBranch :: Instr d l s -> Bool
isBranch (Branch _) = True
isBranch _          = False

mapMem :: (d1 -> d2) -> (s1 -> s2) -> MemInstr d1 s1 -> MemInstr d2 s2
mapMem fd fs = runIdentity . mapMemM (return . fd) (return . fs)

mapAL :: (d1 -> d2) -> (s1 -> s2) -> ALInstr d1 s1 -> ALInstr d2 s2
mapAL fd fs = runIdentity . mapALM (return . fd) (return . fs)

mapB :: (s1 -> s2) -> BranchInstr s1 -> BranchInstr s2
mapB fs = runIdentity . mapBM (return . fs)

mapOut :: (s1 -> s2) -> OutInstr s1 -> OutInstr s2
mapOut fs = runIdentity . mapOutM (return . fs)

mapI :: (d1 -> d2) -> (l1 -> l2) -> (s1 -> s2) -> Instr d1 l1 s1 -> Instr d2 l2 s2
mapI fd fl fs = runIdentity . mapIM (return . fd) (return . fl) (return . fs)

mapIIdx :: (d1 -> d2) -> (l1 -> l2) -> (s1 -> s2) -> (Instr d1 l1 s1, ROBIdx, FreedReg) -> (Instr d2 l2 s2, ROBIdx, FreedReg)
mapIIdx fd fl fs = runIdentity . mapIIdxM (return . fd) (return . fl) (return . fs)

mapMemM :: (Monad m) => (d1 -> m d2) -> (s1 -> m s2) -> MemInstr d1 s1 -> m (MemInstr d2 s2)
mapMemM fd fs (LoadIdx r b off) = do
    b' <- fs b
    r' <- fd r
    return (LoadIdx r' b' off)
mapMemM fd fs (LoadBaseIdx r b off) = do
    b' <- fs b
    o' <- fs off
    r' <- fd r
    return (LoadBaseIdx r' b' o')
mapMemM _ fs (StoreIdx r b off) = do
    b' <- fs b
    r' <- fs r
    return (StoreIdx r' b' off)
mapMemM _ fs (StoreBaseIdx r b off) = do
    b' <- fs b
    o' <- fs off
    r' <- fs r
    return (StoreBaseIdx r' b' o')

mapALM :: (Monad m) => (d1 -> m d2) -> (s1 -> m s2) -> ALInstr d1 s1 -> m (ALInstr d2 s2)
mapALM fd _  (MoveI r i)   = fd r >>= \r' -> return (MoveI r' i)
mapALM fd fs (Move  r x)   = do
    x' <- fs x
    r' <- fd r
    return (Move r' x')
mapALM fd fs (Add   r x y) = mapDSS Add  (fd r) (fs x) (fs y)
mapALM fd fs (AddI  r x i) = mapDSI AddI (fd r) (fs x) i
mapALM fd fs (Sub   r x y) = mapDSS Sub  (fd r) (fs x) (fs y)
mapALM fd fs (SubI  r x i) = mapDSI SubI (fd r) (fs x) i
mapALM fd fs (Mult  r x y) = mapDSS Mult (fd r) (fs x) (fs y)
mapALM fd fs (Div   r x y) = mapDSS Div  (fd r) (fs x) (fs y)
mapALM fd fs (Eq    r x y) = mapDSS Eq   (fd r) (fs x) (fs y)
mapALM fd fs (Lt    r x y) = mapDSS Lt   (fd r) (fs x) (fs y)
mapALM fd fs (Or    r x y) = mapDSS Or   (fd r) (fs x) (fs y)
mapALM fd fs (And   r x y) = mapDSS And  (fd r) (fs x) (fs y)
mapALM fd fs (Not   r x)   = do
    x' <- fs x
    r' <- fd r
    return (Not r' x')

mapDSS :: (Monad m) => (d2 -> s2 -> s2 -> ALInstr d2 s2)
                    -> m d2 -> m s2 -> m s2
                    -> m (ALInstr d2 s2)
mapDSS f dst src1 src2 = do
    src1' <- src1
    src2' <- src2
    dst' <- dst
    return (f dst' src1' src2')

mapDSI :: (Monad m) => (d2 -> s2 -> Val -> ALInstr d2 s2)
                    -> m d2 -> m s2 -> Val
                    -> m (ALInstr d2 s2)
mapDSI f dst src1 imm = do
    src1' <- src1
    dst' <- dst
    return (f dst' src1' imm)

mapBM :: (Monad m) => (s1 -> m s2) -> BranchInstr s1 -> m (BranchInstr s2)
mapBM _  (B addr)    = return (B addr)
mapBM fs (BT r addr) = fs r >>= \r' -> return (BT r' addr)
mapBM fs (BF r addr) = fs r >>= \r' -> return (BF r' addr)
mapBM _  (Ret)       = return Ret
mapBM _  (SysCall)   = return SysCall

mapOutM :: (Monad m) => (s1 -> m s2) -> OutInstr s1 -> m (OutInstr s2)
mapOutM fs (Print  r) = fs r >>= \r' -> return (Print r')
mapOutM fs (PrintC r) = fs r >>= \r' -> return (PrintC r')
mapOutM _  (PrintLn)  = return PrintLn

mapIM :: (Monad m) => (Monad m) => (d1 -> m d2) -> (l1 -> m l2)-> (s1 -> m s2) -> Instr d1 l1 s1 -> m (Instr d2 l2 s2)
mapIM _  fl fs (Mem    i) = mapMemM fl fs i >>= \i' -> return (Mem i')
mapIM fd _  fs (AL     i) = mapALM  fd fs i >>= \i' -> return (AL i')
mapIM _  _  fs (Branch i) = mapBM      fs i >>= \i' -> return (Branch i')
mapIM _  _  fs (Out    i) = mapOutM    fs i >>= \i' -> return (Out i')

mapIIdxM :: (Monad m) => (d1 -> m d2) -> (l1 -> m l2) -> (s1 -> m s2) -> (Instr d1 l1 s1, ROBIdx, FreedReg) -> m (Instr d2 l2 s2, ROBIdx, FreedReg)
mapIIdxM fd fl fs (instr, idx, freed) = do
    instr' <- mapIM fd fl fs instr
    return (instr', idx, freed)
