module Instr where

import Types

data Instr rDst rSrc
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
    | B  { addr :: Addr }               -- Unconditional branch to addr
    | BT { rsrc :: rSrc, addr :: Addr } -- Branch to addr if r == 1
    | BF { rsrc :: rSrc, addr :: Addr } -- Branch to addr if r == 0
    | Ret                               -- Branch to address in link register.
    | SysCall                           -- Terminates the program.
    -- Debugging
    | Print  { rsrc :: rSrc } -- Print value in a register.
    | PrintC { rsrc :: rSrc } -- Prints the value in a register as an ASCII character.
    | PrintLn                 -- Print a newline.
    deriving (Eq, Show)

-- Fetched instruction.
type FInstr = Instr RegIdx RegIdx
-- Decoded instruction.
type DInstr = Instr PhyReg PhyReg
type DInstrIdx = (DInstr, ROBIdx, Maybe PhyReg)
-- Instruction stored in reservation station.
-- Stores partially 'filled-in' instrucions.
type RSInstrIdx = (Instr PhyReg (Either PhyReg Val), ROBIdx, Maybe PhyReg)
-- Executed instruction with computed values filled in.
type EInstr = Instr PhyReg Val
type EInstrIdx = (EInstr, ROBIdx, Maybe PhyReg)

-- Map register and address values stored in instruction.
mapI :: (rDst1 -> rDst2) -> (rSrc1 -> rSrc2) -> (Addr -> Addr) -> Instr rDst1 rSrc1 -> Instr rDst2 rSrc2
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

mapIM :: (Monad m) => (rd1 -> m rd2) -> (rs1 -> m rs2) -> (Addr -> m Addr)
                   -> Instr rd1 rs1
                   -> m (Instr rd2 rs2)
-- Memory
mapIM fd _  _ (MoveI        r i)     = mapRV  MoveI        (fd r) i
mapIM fd fs _ (Move         r from)  = mapRR  Move         (fd r) (fs from)
mapIM fd fs _ (LoadIdx      r b off) = mapRVI LoadIdx      (fd r) (fs b) off
mapIM fd fs _ (LoadBaseIdx  r b off) = mapRVV LoadBaseIdx  (fd r) (fs b) (fs off)
mapIM _  fs _ (StoreIdx     r b off) = mapVVI StoreIdx     (fs r) (fs b) off
mapIM _  fs _ (StoreBaseIdx r b off) = mapVVV StoreBaseIdx (fs r) (fs b) (fs off)
-- Arithmetic/Logic
mapIM fd fs _ (Add  r x y) = mapRVV Add  (fd r) (fs x) (fs y)
mapIM fd fs _ (AddI r x i) = mapRVI AddI (fd r) (fs x) i
mapIM fd fs _ (Sub  r x y) = mapRVV Sub  (fd r) (fs x) (fs y)
mapIM fd fs _ (SubI r x i) = mapRVI SubI (fd r) (fs x) i
mapIM fd fs _ (Mult r x y) = mapRVV Mult (fd r) (fs x) (fs y)
mapIM fd fs _ (Div  r x y) = mapRVV Div  (fd r) (fs x) (fs y)
mapIM fd fs _ (Eq   r x y) = mapRVV Eq   (fd r) (fs x) (fs y)
mapIM fd fs _ (Lt   r x y) = mapRVV Lt   (fd r) (fs x) (fs y)
mapIM fd fs _ (Or   r x y) = mapRVV Or   (fd r) (fs x) (fs y)
mapIM fd fs _ (And  r x y) = mapRVV And  (fd r) (fs x) (fs y)
mapIM fd fs _ (Not  r x)   = mapRR  Not  (fd r) (fs x)
-- Branching
mapIM _ _  fa (B addr)    = fa addr >>= \a -> return (B a)
mapIM _ fs fa (BT r addr) = mapVA BT (fs r) (fa addr)
mapIM _ fs fa (BF r addr) = mapVA BF (fs r) (fa addr)
mapIM _ _  _  (Ret)       = return Ret
mapIM _ _  _  (SysCall)   = return SysCall
-- Debugging
mapIM _ fs _ (Print  r) = mapV Print  (fs r)
mapIM _ fs _ (PrintC r) = mapV PrintC (fs r)
mapIM _ _  _ (PrintLn)  = return PrintLn

mapRV :: (Monad m)
    => (rd -> Val -> Instr rd rs)
    -> m rd -> Val
    -> m (Instr rd rs)
mapRV f r i = do
    r' <- r
    return (f r' i)

mapRR :: (Monad m)
    => (rd -> rs -> Instr rd rs)
    -> m rd -> m rs
    -> m (Instr rd rs)
mapRR f r s = do
    s' <- s
    r' <- r
    return (f r' s')

mapRVI :: (Monad m)
    => (rd -> rs -> Val -> Instr rd rs)
    -> m rd -> m rs -> Val
    -> m (Instr rd rs)
mapRVI f r s i = do
    s' <- s
    r' <- r
    return (f r' s' i)

mapRVV :: (Monad m)
    => (rd -> rs -> rs -> Instr rd rs)
    -> m rd -> m rs -> m rs
    -> m (Instr rd rs)
mapRVV f r s1 s2 = do
    s1' <- s1
    s2' <- s2
    r' <- r
    return (f r' s1' s2')

mapVVI :: (Monad m)
    => (rs -> rs -> Val -> Instr rd rs)
    -> m rs -> m rs -> Val
    -> m (Instr rd rs)
mapVVI f s1 s2 i = do
    s1' <- s1
    s2' <- s2
    return (f s1' s2' i)

mapVVV :: (Monad m)
    => (rs -> rs -> rs -> Instr rd rs)
    -> m rs -> m rs -> m rs
    -> m (Instr rd rs)
mapVVV f s1 s2 s3 = do
    s1' <- s1
    s2' <- s2
    s3' <- s3
    return (f s1' s2' s3')

mapVA :: (Monad m)
    => (rs -> Addr -> Instr rd rs)
    -> m rs -> m Addr
    -> m (Instr rd rs)
mapVA f s a = do
    s' <- s
    a' <- a
    return (f s' a')

mapV :: (Monad m)
    => (rs -> Instr rd rs)
    -> m rs
    -> m (Instr rd rs)
mapV f s = do
    s' <- s
    return (f s')

-- Map the instruction stored with a reorder buffer index.
mapIIdx :: (rDst1 -> rDst2) -> (rSrc1 -> rSrc2) -> (Addr -> Addr)
        -> (Instr rDst1 rSrc1, ROBIdx, FreedReg)
        -> (Instr rDst2 rSrc2, ROBIdx, FreedReg)
mapIIdx fd fs fa (instr, idx, freed) = (mapI fd fs fa instr, idx, freed)

mapIIdxM :: (Monad m) => (rd1 -> m rd2) -> (rs1 -> m rs2) -> (Addr -> m Addr)
                      -> (Instr rd1 rs1, ROBIdx, FreedReg)
                      -> m (Instr rd2 rs2, ROBIdx, FreedReg)
mapIIdxM fd fs fa (instr, idx, freed) = do
    instr' <- mapIM fd fs fa instr
    return (instr', idx, freed)

isMem :: Instr rDst rSrc -> Bool
isMem (LoadIdx      _ _ _) = True
isMem (LoadBaseIdx  _ _ _) = True
isMem (StoreIdx     _ _ _) = True
isMem (StoreBaseIdx _ _ _) = True
isMem _                    = False

isAL :: Instr rDst rSrc -> Bool
isAL (MoveI _ _)   = True
isAL (Move  _ _)   = True
isAL (Add   _ _ _) = True
isAL (AddI  _ _ _) = True
isAL (Sub   _ _ _) = True
isAL (SubI  _ _ _) = True
isAL (Mult  _ _ _) = True
isAL (Div   _ _ _) = True
isAL (Eq    _ _ _) = True
isAL (Lt    _ _ _) = True
isAL (Or    _ _ _) = True
isAL (And   _ _ _) = True
isAL (Not   _ _)   = True
isAL _            = False

isBranch :: Instr rDst rSrc -> Bool
isBranch (B _)     = True
isBranch (BT _ _)  = True
isBranch (BF _ _)  = True
isBranch (Ret)     = True
isBranch (SysCall) = True
isBranch _         = False

isOut :: Instr rDst rSrc -> Bool
isOut (Print  _) = True
isOut (PrintC _) = True
isOut (PrintLn)  = True
isOut _          = False
