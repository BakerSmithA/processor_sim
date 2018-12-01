module Decode where

import Instr
import State (State, Res)
import qualified State as St
import Control.Monad.Trans.Maybe

-- Because instruction are already parsed into struct, no need to decode.
-- However, register renaming will be performed at this step.
-- decode :: FInstr -> State -> Res (Maybe DInstr, State)
-- decode i st = return (Just i, st)

-- Because instruction are already parsed into struct, no need to decode.
-- However, register renaming will be performed at this step.
decode :: FInstr -> State -> Res (Maybe DInstr, State)
decode fi st = fmap (fmap addSt) runMaybeT (decodeI fi st) where
    addSt = maybe (Nothing, st) (\(di, st') -> (Just di, st'))

decodeI :: FInstr -> State -> MaybeT Res (DInstr, State)
decodeI (MoveI r i)           = decodeRI MoveI r i
decodeI (Move r from)         = decodeRR Move  r from
decodeI (LoadIdx r b off)     = decodeRRI LoadIdx r b off
decodeI (LoadBaseIdx r b off) = decodeRRR LoadBaseIdx r b off
-- Stores cannot use decodeXXX functions because the their 'r' register is not
-- treated as a destination register, and so does not need renaming/
decodeI (StoreIdx r b off) = \st -> do
    pr   <- getPReg r st
    pb   <- getPReg b st
    return (StoreIdx pr pb off, st)
decodeI (StoreBaseIdx r b off) = \st -> do
    pr   <- getPReg r st
    pb   <- getPReg b st
    poff <- getPReg off st
    return (StoreBaseIdx pr pb poff, st)

decodeI (Add  r x y) = decodeRRR Add  r x y
decodeI (AddI r x i) = decodeRRI AddI r x i
decodeI (Sub  r x y) = decodeRRR Sub  r x y
decodeI (SubI r x i) = decodeRRI SubI r x i
decodeI (Mult r x y) = decodeRRR Mult r x y
decodeI (Div  r x y) = decodeRRR Div  r x y
decodeI (Eq   r x y) = decodeRRR Eq   r x y
decodeI (Lt   r x y) = decodeRRR Lt   r x y
decodeI (Or   r x y) = decodeRRR Or   r x y
decodeI (And  r x y) = decodeRRR And  r x y
decodeI (Not  r x)   = decodeRR  Not  r x

decodeI (B addr)    = pass (B addr)
decodeI (BT r addr) = decodeRAddr BT r addr
decodeI (BF r addr) = decodeRAddr BF r addr
decodeI (Ret)       = pass Ret

decodeI (Print r) = \st -> do
    p <- getPReg r st
    return (Print p, st)
decodeI (PrintLn) = pass PrintLn

decodeI (SysCall) = pass SysCall

-- Convenience function for decoding branch instructions that are dependent
-- on a register value.
decodeRAddr :: (PhyReg -> Addr -> DInstr)
             -> RegIdx -> Addr
             -> State
             -> MaybeT Res (DInstr, State)
decodeRAddr f r addr st = do
    pr <- getPReg r st
    return (f pr addr, st)

-- Convenience function for decoding instructions consisting of a destination
-- register and two source registers.
decodeRRR :: (PhyReg -> PhyReg -> PhyReg -> DInstr)
           -> RegIdx -> RegIdx -> RegIdx
           -> State
           -> MaybeT Res (DInstr, State)
decodeRRR f r x y = withPReg r $ \p st -> do
    px <- getPReg x st
    py <- getPReg y st
    return (f p px py)

-- Convenience function for decoding instructions consisting of a destination
-- register, a source register, and an immediate value.
decodeRRI :: (PhyReg -> PhyReg -> Val -> DInstr)
           -> RegIdx -> RegIdx -> Val
           -> State
           -> MaybeT Res (DInstr, State)
decodeRRI f r x i = withPReg r $ \p st -> do
    px <- getPReg x st
    return (f p px i)

-- Convenience function for decoding instructions with destination and single operand.
decodeRR :: (PhyReg -> PhyReg -> DInstr)
          -> RegIdx -> RegIdx
          -> State
          -> MaybeT Res (DInstr, State)
decodeRR f r x = withPReg r $ \p st -> do
    px <- getPReg x st
    return (f p px)

-- Convenience function for decoding instruction with destination and immediate.
decodeRI :: (PhyReg -> Val -> DInstr)
          -> RegIdx -> Val
          -> State
          -> MaybeT Res (DInstr, State)
decodeRI f r i = withPReg r $ \p _ -> do
    return (f p i)

pass :: DInstr -> State -> MaybeT Res (DInstr, State)
pass i st = return (i, st)

-- Takes a physical register and handles state.
withPReg :: RegIdx
        -> (PhyReg -> State -> MaybeT Res DInstr)
        -> State
        -> MaybeT Res (DInstr, State)
withPReg r f st1 = do
    (pr, st2) <- takePReg r st1
    fmap (\di -> (di, st2)) (f pr st2)

-- Convenience function for allocating a physical register.
takePReg :: RegIdx -> State -> MaybeT Res (PhyReg, State)
takePReg r st = MaybeT (return (St.allocPhyReg r st))

-- Convenience function for getting the mapping to a physical register.
getPReg :: RegIdx -> State -> MaybeT Res PhyReg
getPReg r st = MaybeT (fmap Just (St.getPhyReg r st))
