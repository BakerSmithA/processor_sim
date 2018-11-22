module Exec.LSU where

import Exec.Unit
import qualified Mem as Mem

loadStoreUnit :: ExecUnit
loadStoreUnit = unit ls where
    ls (MoveI        r i)     (EmptyOp)     st = writeReg r i
    ls (Move         r _)     (UniOp x)     st = writeReg r x
    ls (LoadIdx      r _ off) (UniOp base)  st = load base off st >>= writeReg r
    ls (LoadBaseIdx  r _ _)   (BinOp b off) st = undefined
    ls (StoreIdx     r _ off) (UniOp base)  st = undefined
    ls (StoreBaseIdx r _ _)   (BinOp b off) st = undefined

    ls _ _ _ = error "unexpected ls"

-- Attempt to load value from memory, may fail if address is out of range.
load :: Val -> Val -> State -> Result Val
load base offset st =
    let addr = fromIntegral (base + offset)
    in case Mem.load addr (mem st) of
        Nothing  -> undefined
        Just val -> return val
