module VMSpec (vmSpec) where

import Test.Hspec
import Instr
import State as State
import VM as VM
import qualified Mem as Mem
import qualified Mem as Reg
import Data.Word (Word32)

-- Return instructions which terminate.
term :: [Instr] -> [Instr]
term is = is ++ [SysCall]

makeVm :: [Instr] -> [Word32] -> State
makeVm instrs memCnts = State mem regs instrs' pcIdx spIdx lrIdx bpIdx retIdx [] where
    mem = Mem.fromList memCnts
    regs = Reg.zeroed 10
    instrs' = Mem.fromList (term instrs)
    pcIdx  = 6
    spIdx  = 7
    lrIdx  = 8
    bpIdx  = 9
    retIdx = 10

vmSpec :: Spec
vmSpec = describe "vm" $ do
    context "memory" $ do
        it "interprets MoveI" $ do
            let vm = makeVm [MoveI 1 5] []
                vm' = run vm
            (vm' >>= VM.regVal 1) `shouldBe` VM 5
