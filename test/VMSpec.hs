module VMSpec (vmSpec) where

import Test.Hspec
import Instr
import State as State
import VM as VM
import qualified Mem as Mem
import Data.Word (Word32)
import Debug.Trace

runVm :: [Instr] -> [Word32] -> State
runVm instrs memCnts = run $ State mem regs instrs' pcIdx spIdx lrIdx bpIdx retIdx [] where
    mem = Mem.fromList memCnts
    regs = Mem.zeroed 10
    instrs' = Mem.fromList (instrs ++ [SysCall])
    pcIdx  = 6
    spIdx  = 7
    lrIdx  = 8
    bpIdx  = 9
    retIdx = 10

vmSpec :: Spec
vmSpec = describe "vm" $ do
    context "normal execution" $ do
        context "memory" $ do
            it "interprets Exit" $ do
                let vm = runVm [] []
                vm `shouldBe` vm

            it "interprets MoveI" $ do
                let vm = runVm [MoveI 1 5] []
                VM.regVal 1 vm `shouldBe` VM 5

            it "interprets Move" $ do
                let vm = runVm [MoveI 0 6, Move 1 0] []
                VM.regVal 1 vm `shouldBe` VM 6
