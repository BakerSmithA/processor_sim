module Exec.BU where

import Exec.Unit

branchUnit :: ExecUnit
branchUnit = unit b where
    b (B addr)    (EmptyOp) st = branch addr st
    b (BT _ addr) (UniOp x) st = branchCond (x==1) addr st
    b (BF _ addr) (UniOp x) st = branchCond (x/=1) addr st
    b (Ret)       (EmptyOp) st = undefined
    b (SysCall)   (EmptyOp) st = undefined

    b _ _ _ = error "unexpected branch"

-- Executes a branch by writing PC.
branch :: Addr -> State -> Result WriteBackInstr
branch addr st = writeReg pc addr' where
    pc = pcIdx st
    -- +1 because pipeline stalls until branch executed, and PC not updated.
    addr' = fromIntegral (addr+1)

-- Executes a branch if the value in a register passes a condition, otherwise NoOp.
branchCond :: Bool -> Addr -> State -> Result WriteBackInstr
branchCond cond addr st = do
    if cond
        then branch addr st
        else return NoOp
