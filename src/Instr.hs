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
    = ELoadIdx      PhyReg Val Addr
    | ELoadBaseIdx  PhyReg Val Addr
    | EStoreIdx     Val    Addr
    | EStoreBaseIdx Val    Addr
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

-- Fetched AL instruction.
type FALInstr = ALInstr RegIdx RegIdx
-- Decoded AL instruction.
type DALInstr = ALInstr PhyReg PhyReg
-- AL instruction with partly filled in operands in reservation station.
type RSALInstr = ALInstr PhyReg (Either PhyReg Val)
-- AL instruction ready to be executed.
type EALInstr = ALInstr PhyReg Val

data BranchInstr rSrc
    = B       Addr -- Unconditional branch to addr
    | BT rSrc Addr -- Branch to addr if r == 1
    | BF rSrc Addr -- Branch to addr if r == 0
    | Ret          -- Branch to address in link register.
    | SysCall      -- Terminates the program.
    deriving (Eq, Show)

-- Fetched branch instruction.
type FBranchInstr = BranchInstr RegIdx
-- Decoded branch instruction.
type DBranchInstr = BranchInstr PhyReg
-- Branch instruction with partly filled in operands in reservation station.
type RSBranchInstr = BranchInstr (Either PhyReg Val)
-- Branch instruction from RS that is ready for execution.
data EBranchInstr
    = EB  Addr
    | EBT Val Addr
    | EBF Val Addr
    | ERet Addr -- Value of LR retrieved from memory.
    | ESysCall
    deriving (Eq, Show)

data OutInstr rSrc
    = Print  rSrc -- Print value in a register.
    | PrintC rSrc -- Prints the value in a register as an ASCII character.
    | PrintLn     -- Print a newline.
    deriving (Eq, Show)

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

type SameInstr r s = Instr (MemInstr r s) (ALInstr r s) (BranchInstr s) (OutInstr s)

-- Fetched instruction.
type FInstr = Instr FMemInstr FALInstr FBranchInstr FOutInstr
-- Decoded instruction.
type DInstr = Instr DMemInstr DALInstr DBranchInstr DOutInstr
-- Instruction in reservation station.
type RSInstr = Instr RSMemInstr RSALInstr RSBranchInstr RSOutInstr
-- Instruction to execute.
type EInstr = Instr EMemInstr EALInstr EBranchInstr EOutInstr

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

b :: Addr -> Instr mem al (BranchInstr s) out
b addr = Branch (B addr)

bt :: s -> Addr -> Instr mem al (BranchInstr s) out
bt r addr = Branch (BT r addr)

bf :: s -> Addr -> Instr mem al (BranchInstr s) out
bf r addr = Branch (BF r addr)

ret :: Instr mem al (BranchInstr s) out
ret = Branch Ret

sysCall :: Instr mem al (BranchInstr s) out
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
