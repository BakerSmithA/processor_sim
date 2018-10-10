module VMSpec (vmSpec) where

import Test.Hspec
import qualified Mem as Mem
import qualified Reg as Reg
import Instr
import VM as VM

makeVm :: [Instr] -> VM
makeVm instrs = VM mem regs instrs' pcIdx spIdx lrIdx where
    mem = Mem.zeroed 16
    regs = Reg.file 8
    instrs' = Mem.fromList instrs
    pcIdx = 6
    spIdx = 7
    lrIdx = 8

vmSpec :: Spec
vmSpec = describe "vm" $ do
    context "interpreting instructions" $ do
        it "interprets MoveI" $ do
            pending
