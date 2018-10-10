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
    let instrs = [MoveI 0 2, MoveI 1 3, Add 2 0 1]
    let vm = makeVm instrs
    putStrLn (show vm)
    let vm' = run vm
    putStrLn (show vm')
