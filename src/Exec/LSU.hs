module Exec.LSU where

import Exec.Unit

loadStoreUnit :: ExecUnit
loadStoreUnit = unit ls where
    ls (MoveI        r i)     (EmptyOp)     _ = writeReg r i
    ls (Move         r _)     (UniOp x)     _ = writeReg r x
    ls (LoadIdx      r _ off) (UniOp base)  _ = undefined
    ls (LoadBaseIdx  r _ _)   (BinOp b off) _ = undefined
    ls (StoreIdx     r _ off) (UniOp base)  _ = undefined
    ls (StoreBaseIdx r _ _)   (BinOp b off) _ = undefined

    ls _ _ _ = error "unexpected ls"
