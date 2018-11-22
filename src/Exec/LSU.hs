module Exec.LSU where

import Exec.Unit
import qualified Mem as Mem
import Error

loadStoreUnit :: ExecUnit
loadStoreUnit = unit ls where
    ls (MoveI        r i)     (EmptyOp)       st = writeReg r i
    ls (Move         r _)     (UniOp x)       st = writeReg r x
    ls (LoadIdx      r _ off) (UniOp base)    st = load r base off st
    ls (LoadBaseIdx  r _ _)   (BinOp b off)   st = load r b off st
    ls (StoreIdx     _ _ off) (BinOp x base)  st = undefined
    ls (StoreBaseIdx _ _ _)   (BinOp x b off) st = undefined

    ls _ _ _ = error "unexpected ls"

-- Attempt to load value from memory into register, may fail if address is out
-- of range.
load :: RegIdx -> Val -> Val -> State -> Result WriteBackInstr
load reg base offset st =
    let addr = fromIntegral (base + offset)
    in case Mem.load addr (mem st) of
        Nothing  -> crash (MemOutOfRange addr) st
        Just val -> writeReg reg val

-- Attempt to store value from register into memory.
store :: Val -> Val -> Val -> Result WriteBackInstr
store base offset val = return (WriteMem addr val) where
    addr = fromIntegral (base + offset)
