module Instr where

import Control.Monad.Identity
import Types

data MemInstr rDst rSrc
    = LoadIdx      rDst rSrc Val  -- r <- [[base] + offset]
    | LoadBaseIdx  rDst rSrc rSrc -- r <- [[base] + [R_offset]]
    | StoreIdx     rSrc rSrc Val  -- r -> [[base] + offset]
    | StoreBaseIdx rSrc rSrc rSrc -- r -> [[base] + [R_offset]]
    deriving (Eq, Show)

-- Fetched memory instruction.
type FMemInstr = MemInstr RegIdx RegIdx
-- Decoded memory instruction.
type DMemInstr = MemInstr PhyReg PhyReg
-- Memory instruction with partly filled in operands in reservation station.
type RSMemInstr = MemInstr (PhyReg, Maybe Val) (Either PhyReg Val)
-- Memory instruction from RS that is ready to be executed.
-- Stored with the address to access.
data EMemInstr
    = ELoad PhyReg Val Addr
    | EStore Val Addr
    deriving (Eq, Show)

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

mapMem :: (d1 -> d2) -> (s1 -> s2) -> MemInstr d1 s1 -> MemInstr d2 s2
mapMem fd fs = runIdentity . mapMemM (return . fd) (return . fs)

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

-- Fetched AL instruction.
type FALInstr = ALInstr RegIdx RegIdx
-- Decoded AL instruction.
type DALInstr = ALInstr PhyReg PhyReg
-- AL instruction with partly filled in operands in reservation station.
type RSALInstr = ALInstr PhyReg (Either PhyReg Val)
-- AL instruction ready to be executed.
type EALInstr = ALInstr PhyReg Val

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
    dst'  <- dst
    return (f dst' src1' src2')

mapDSI :: (Monad m) => (d2 -> s2 -> Val -> ALInstr d2 s2)
                    -> m d2 -> m s2 -> Val
                    -> m (ALInstr d2 s2)
mapDSI f dst src1 imm = do
    src1' <- src1
    dst' <- dst
    return (f dst' src1' imm)

mapAL :: (d1 -> d2) -> (s1 -> s2) -> ALInstr d1 s1 -> ALInstr d2 s2
mapAL fd fs = runIdentity . mapALM (return . fd) (return . fs)

data BranchInstr rSrc retSrc
    = B          Addr -- Unconditional branch to addr
    | BT  rSrc   Addr -- Branch to addr if r == 1
    | BF  rSrc   Addr -- Branch to addr if r == 0
    | Ret retSrc      -- Branch to address in link register.
    | SysCall         -- Terminates the program.
    deriving (Eq, Show)

-- Fetched branch instruction.
type FBranchInstr = BranchInstr RegIdx ()
-- Decoded branch instruction.
type DBranchInstr = BranchInstr PhyReg PhyReg
-- Branch instruction with partly filled in operands in reservation station.
type RSBranchInstr = BranchInstr (Either PhyReg Val) (Either PhyReg Val)
-- Branch instruction from RS that is ready for execution.
type EBranchInstr = BranchInstr Val Val

mapBM :: (Monad m) => (s1 -> m s2) -> (retS1 -> m retS2) -> BranchInstr s1 retS1 -> m (BranchInstr s2 retS2)
mapBM _  _    (B addr)    = return (B addr)
mapBM fs _    (BT r addr) = fs r   >>= \r' -> return (BT r' addr)
mapBM fs _    (BF r addr) = fs r   >>= \r' -> return (BF r' addr)
mapBM _  fret (Ret r)     = fret r >>= \r' -> return (Ret r')
mapBM _  _    (SysCall)   = return SysCall

mapB :: (s1 -> s2) -> (retS1 -> retS2) -> BranchInstr s1 retS1 -> BranchInstr s2 retS2
mapB fs fret = runIdentity . mapBM (return . fs) (return . fret)

data OutInstr rSrc
    = Print  rSrc -- Print value in a register.
    | PrintC rSrc -- Prints the value in a register as an ASCII character.
    | PrintLn     -- Print a newline.
    deriving (Eq, Show)

mapOutM :: (Monad m) => (s1 -> m s2) -> OutInstr s1 -> m (OutInstr s2)
mapOutM fs (Print  r) = fs r >>= \r' -> return (Print r')
mapOutM fs (PrintC r) = fs r >>= \r' -> return (PrintC r')
mapOutM _  (PrintLn)  = return PrintLn

mapOut :: (s1 -> s2) -> OutInstr s1 -> OutInstr s2
mapOut fs = runIdentity . mapOutM (return . fs)

-- Fetched out instruction.
type FOutInstr = OutInstr RegIdx
-- Decoded out instruction.
type DOutInstr = OutInstr PhyReg
-- Out instruction with partly filled in operands in reservation station.
type RSOutInstr = OutInstr (Either PhyReg Val)
-- Out instruction that is ready to execute.
type EOutInstr = OutInstr Val

data Instr mem al b out
    = Mem mem
    | AL al
    | Branch b
    | Out out
    deriving (Eq, Show)

type SameInstr d s retS = Instr (MemInstr d s) (ALInstr d s) (BranchInstr s retS) (OutInstr s)

-- Fetched instruction.
type FInstr = Instr FMemInstr FALInstr FBranchInstr FOutInstr
-- Decoded instruction.
type DInstr = Instr DMemInstr DALInstr DBranchInstr DOutInstr
-- Instruction in reservation station.
type RSInstr = Instr RSMemInstr RSALInstr RSBranchInstr RSOutInstr
-- Instruction to execute.
type EInstr = Instr EMemInstr EALInstr EBranchInstr EOutInstr

mapIM :: (Monad m) => (Monad m) => (d1 -> m d2) -> (s1 -> m s2) -> (retS1 -> m retS2) -> SameInstr d1 s1 retS1 -> m (SameInstr d2 s2 retS2)
mapIM fd fs _    (Mem    i) = mapMemM fd fs      i >>= \i' -> return (Mem i')
mapIM fd fs _    (AL     i) = mapALM  fd fs      i >>= \i' -> return (AL i')
mapIM _  fs fret (Branch i) = mapBM      fs fret i >>= \i' -> return (Branch i')
mapIM _  fs _    (Out    i) = mapOutM    fs      i >>= \i' -> return (Out i')

mapI :: (d1 -> d2) -> (s1 -> s2) -> (retS1 -> retS2) -> SameInstr d1 s1 retS1 -> SameInstr d2 s2 retS2
mapI fd fs fret = runIdentity . mapIM (return . fd) (return . fs) (return . fret)

-- Used to store instructions with accompanying data as they pass through the
-- pipeline.
type PipeData i = (i, ROBIdx, FreedReg)

type FPipeInstr  = PipeData FInstr
type DPipeInstr  = PipeData DInstr
type RSPipeInstr = PipeData RSInstr
type EPipeInstr  = PipeData EInstr

mapPipeIM :: (Monad m) => (d1 -> m d2) -> (s1 -> m s2) -> (retS1 -> m retS2) -> PipeData (SameInstr d1 s1 retS1) -> m (PipeData (SameInstr d2 s2 retS2))
mapPipeIM fd fs fret (i, idx, freed) = do
    i' <- mapIM fd fs fret i
    return (i', idx, freed)

mapPipeI :: (d1 -> d2) -> (s1 -> s2) -> (retS1 -> retS2) -> PipeData (SameInstr d1 s1 retS1) -> PipeData (SameInstr d2 s2 retS2)
mapPipeI fd fs fret = runIdentity . mapPipeIM (return . fd) (return . fs) (return . fret)

--------------------------------------------------------------------------------
-- Convenience methods for constructing instructions.
--------------------------------------------------------------------------------

loadIdx :: d -> s -> Val -> Instr (MemInstr d s) al b out
loadIdx r base off = Mem (LoadIdx r base off)

loadBaseIdx :: d -> s -> s -> Instr (MemInstr d s) al b out
loadBaseIdx r base off = Mem (LoadBaseIdx r base off)

storeIdx :: s -> s -> Val -> Instr (MemInstr d s) al b out
storeIdx r base off = Mem (StoreIdx r base off)

storeBaseIdx :: s -> s -> s -> Instr (MemInstr d s) al b out
storeBaseIdx r base off = Mem (StoreBaseIdx r base off)

moveI :: d -> Val -> Instr mem (ALInstr d s) b out
moveI r i = AL (MoveI r i)

move :: d -> s -> Instr mem (ALInstr d s) b out
move r x = AL (Move r x)

add :: d -> s -> s -> Instr mem (ALInstr d s) b out
add r x y = AL (Add r x y)

addI :: d -> s -> Val -> Instr mem (ALInstr d s) b out
addI r x i = AL (AddI r x i)

sub :: d -> s -> s -> Instr mem (ALInstr d s) b out
sub r x y = AL (Sub r x y)

subI :: d -> s -> Val -> Instr mem (ALInstr d s) b out
subI r x i = AL (SubI r x i)

mult :: d -> s -> s -> Instr mem (ALInstr d s) b out
mult r x y = AL (Mult r x y)

divI :: d -> s -> s -> Instr mem (ALInstr d s) b out
divI r x y = AL (Div r x y)

eq :: d -> s -> s -> Instr mem (ALInstr d s) b out
eq r x y = AL (Eq r x y)

lt :: d -> s -> s -> Instr mem (ALInstr d s) b out
lt r x y = AL (Lt r x y)

orI :: d -> s -> s -> Instr mem (ALInstr d s) b out
orI r x y = AL (Or r x y)

andI :: d -> s -> s -> Instr mem (ALInstr d s) b out
andI r x y = AL (And r x y)

notI :: d -> s -> Instr mem (ALInstr d s) b out
notI r x = AL (Not r x)

b :: Addr -> Instr mem al (BranchInstr s retS) out
b addr = Branch (B addr)

bt :: s -> Addr -> Instr mem al (BranchInstr s retS) out
bt r addr = Branch (BT r addr)

bf :: s -> Addr -> Instr mem al (BranchInstr s retS) out
bf r addr = Branch (BF r addr)

ret :: retS -> Instr mem al (BranchInstr s retS) out
ret = Branch . Ret

sysCall :: Instr mem al (BranchInstr s retS) out
sysCall = Branch SysCall

printI :: s -> Instr mem al b (OutInstr s)
printI r = Out (Print r)

printC :: s -> Instr mem al b (OutInstr s)
printC r = Out (PrintC r)

printLn :: Instr mem al b (OutInstr s)
printLn = Out (PrintLn)

isBranch :: Instr mem al b out -> Bool
isBranch (Branch _) = True
isBranch _          = False
