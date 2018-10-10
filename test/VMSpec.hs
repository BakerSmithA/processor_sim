module VMSpec (vmSpec) where

import Test.Hspec
import qualified Mem as Mem
import qualified Reg as Reg
import Instr
import VM as VM
import Data.Word (Word32)

makeVm :: [Instr] -> [Word32] -> VM
makeVm instrs memCnts = VM mem regs instrs' pcIdx spIdx lrIdx where
    mem = Mem.fromList memCnts
    regs = Reg.file 8
    instrs' = Mem.fromList instrs
    pcIdx = 6
    spIdx = 7
    lrIdx = 8

vmSpec :: Spec
vmSpec = describe "vm" $ do
    describe "interpreting instructions" $ do
        context "memory instructions" $ do
            it "interprets MoveI" $ do
                let vm  = makeVm [MoveI 1 5] []
                    vm' = run vm
                (VM.reg 1 vm') `shouldBe` 5

            it "interprets LoadIdx" $ do
                let vm  = makeVm [MoveI 0 1, LoadIdx 1 0 2] [1, 2, 3, 4, 5]
                    vm' = run vm
                (VM.reg 1 vm') `shouldBe` 4

            it "interprets LoadBaseIdx" $ do
                let vm  = makeVm [MoveI 0 1, MoveI 1 3, LoadBaseIdx 2 0 1] [1, 2, 3, 4, 5]
                    vm' = run vm
                (VM.reg 2 vm') `shouldBe` 5

            it "interprets StoreIdx" $ do
                let vm  = makeVm [MoveI 0 7, MoveI 1 2, StoreIdx 0 1 2] [0, 0, 0, 0, 0]
                    vm' = run vm
                (VM.mem vm') `shouldBe` Mem.fromList [0, 0, 0, 0, 7]

            it "interprets StoreBaseIdx" $ do
                let vm  = makeVm [MoveI 0 7, MoveI 1 2, MoveI 2 3, StoreBaseIdx 0 1 2] [0, 0, 0, 0, 0, 0]
                    vm' = run vm
                (VM.mem vm') `shouldBe` Mem.fromList [0, 0, 0, 0, 0, 7]
