module Decode where

import Instr
import State (State, Res)
import qualified State as St
import Control.Monad.Trans.Maybe

-- Because instruction are already parsed into struct, no need to decode.
-- However, register renaming will be performed at this step.
-- decode :: FInstr -> State -> Res (DInstr, State)
-- decode i st = return (i, st)

-- Because instruction are already parsed into struct, no need to decode.
-- However, register renaming will be performed at this step.
decode :: FInstr -> State -> Res (DInstr, State)
decode (MoveI r i)           = decodeRI  MoveI       r i
decode (Move r from)         = decodeRR  Move        r from
decode (LoadIdx r b off)     = decodeRRI LoadIdx     r b off
decode (LoadBaseIdx r b off) = decodeRRR LoadBaseIdx r b off
-- Stores cannot use decodeXXX functions because the their 'r' register is not
-- treated as a destination register, and so does not need renaming/
decode (StoreIdx r b off) = \st -> do
    pr   <- St.getPhyReg r st
    pb   <- St.getPhyReg b st
    return (StoreIdx pr pb off, st)
decode (StoreBaseIdx r b off) = \st -> do
    pr   <- St.getPhyReg r st
    pb   <- St.getPhyReg b st
    poff <- St.getPhyReg off st
    return (StoreBaseIdx pr pb poff, st)

decode (Add  r x y) = decodeRRR Add  r x y
decode (AddI r x i) = decodeRRI AddI r x i
decode (Sub  r x y) = decodeRRR Sub  r x y
decode (SubI r x i) = decodeRRI SubI r x i
decode (Mult r x y) = decodeRRR Mult r x y
decode (Div  r x y) = decodeRRR Div  r x y
decode (Eq   r x y) = decodeRRR Eq   r x y
decode (Lt   r x y) = decodeRRR Lt   r x y
decode (Or   r x y) = decodeRRR Or   r x y
decode (And  r x y) = decodeRRR And  r x y
decode (Not  r x)   = decodeRR  Not  r x

decode (B addr)    = pass (B addr)
decode (BT r addr) = decodeRAddr BT r addr
decode (BF r addr) = decodeRAddr BF r addr
decode (Ret)       = pass Ret

decode (Print r) = \st -> do
    p <- St.getPhyReg r st
    return (Print p, st)
decode (PrintLn) = pass PrintLn

decode (SysCall) = pass SysCall

-- Convenience function for decoding branch instructions that are dependent
-- on a register value.
decodeRAddr :: (PhyReg -> Addr -> DInstr)
             -> RegIdx -> Addr
             -> State
             -> Res (DInstr, State)
decodeRAddr f r addr st = do
    pr <- St.getPhyReg r st
    return (f pr addr, st)

-- Convenience function for decoding instructions consisting of a destination
-- register and two source registers.
decodeRRR :: (PhyReg -> PhyReg -> PhyReg -> DInstr)
           -> RegIdx -> RegIdx -> RegIdx
           -> State
           -> Res (DInstr, State)
decodeRRR f r x y = withPReg r $ \p st -> do
    px <- St.getPhyReg x st
    py <- St.getPhyReg y st
    return (f p px py)

-- Convenience function for decoding instructions consisting of a destination
-- register, a source register, and an immediate value.
decodeRRI :: (PhyReg -> PhyReg -> Val -> DInstr)
           -> RegIdx -> RegIdx -> Val
           -> State
           -> Res (DInstr, State)
decodeRRI f r x i = withPReg r $ \p st -> do
    px <- St.getPhyReg x st
    return (f p px i)

-- Convenience function for decoding instructions with destination and single operand.
decodeRR :: (PhyReg -> PhyReg -> DInstr)
          -> RegIdx -> RegIdx
          -> State
          -> Res (DInstr, State)
decodeRR f r x = withPReg r $ \p st -> do
    px <- St.getPhyReg x st
    return (f p px)

-- Convenience function for decoding instruction with destination and immediate.
decodeRI :: (PhyReg -> Val -> DInstr)
          -> RegIdx -> Val
          -> State
          -> Res (DInstr, State)
decodeRI f r i = withPReg r $ \p _ -> do
    return (f p i)

pass :: DInstr -> State -> Res (DInstr, State)
pass i st = return (i, st)

-- Takes a physical register and handles state.
withPReg :: RegIdx
        -> (PhyReg -> State -> Res DInstr)
        -> State
        -> Res (DInstr, State)
withPReg r f st1 = do
    (pr, st2) <- St.allocPhyReg r st1
    fmap (\di -> (di, st2)) (f pr st2)
