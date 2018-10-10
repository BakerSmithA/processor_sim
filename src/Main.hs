module Main where

import qualified Mem as Mem
import qualified Reg as Reg
import Instr
import VM as VM

makeVm :: [Instr] -> VM
makeVm instrs = VM mem regs instrs' pcIdx spIdx lrIdx where
    mem = Mem.zeroed 1
    regs = Reg.file 8
    instrs' = Mem.fromList instrs
    pcIdx = 6
    spIdx = 7
    lrIdx = 8

main :: IO ()
main = do
    let instrs = [MoveI 0 10, MoveI 1 0, SubI 0 0 1, AddI 1 1 1, BLT 0 2]
    let vm = makeVm instrs
    putStrLn (show vm)
    let vm' = run vm
    putStrLn (show vm')
