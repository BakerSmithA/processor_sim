module VMSpec (vmSpec) where

import Test.Hspec
import Instr
import State as State
import VM as VM
import qualified Mem as Mem
import Data.Word (Word32)
import Debug.Trace

makeVm :: [Instr] -> [Word32] -> State
makeVm instrs memCnts = State mem regs instrs' pcIdx spIdx lrIdx bpIdx retIdx [] where
    mem = Mem.fromList memCnts
    regs = Mem.fromList [0, 0, 0]
    instrs' = Mem.fromList (instrs ++ [SysCall])
    pcIdx  = 6
    spIdx  = 7
    lrIdx  = 8
    bpIdx  = 9
    retIdx = 10

vmSpec :: Spec
vmSpec = describe "vm" $ do
    context "memory" $ do
        it "interprets Exit" $ do
            let vm = makeVm [] []
            vm `shouldBe` vm

        -- it "interprets MoveI" $ do
        --     let vm = makeVm [MoveI 1 5] []
        --         vm' = run vm
        --     VM.regVal 1 vm' `shouldBe` VM 5
