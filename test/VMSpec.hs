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

            it "interprets Move" $ do
                let vm  = makeVm [MoveI 0 6, Move 1 0] []
                    vm' = run vm
                (VM.reg 1 vm') `shouldBe` 6

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

        context "ALU instructions" $ do
            it "interprets Add" $ do
                let vm  = makeVm [MoveI 0 1, MoveI 1 2, Add 2 0 1] []
                    vm' = run vm
                (VM.reg 2 vm') `shouldBe` 3

            it "interprets Sub" $ do
                let vm  = makeVm [MoveI 0 5, MoveI 1 3, Sub 2 0 1] []
                    vm' = run vm
                (VM.reg 2 vm') `shouldBe` 2

            it "interprets Eq to be True" $ do
                let vm  = makeVm [MoveI 0 1, MoveI 1 1, Eq 2 0 1] []
                    vm' = run vm
                (VM.reg 2 vm') `shouldBe` 1

            it "interprets Eq to be False" $ do
                let vm  = makeVm [MoveI 0 1, MoveI 1 2, Eq 2 0 1] []
                    vm' = run vm
                (VM.reg 2 vm') `shouldBe` 0

            it "interprets Or to be True" $ do
                let vm  = makeVm [MoveI 0 0, MoveI 1 1, Or 2 0 1] []
                    vm' = run vm
                (VM.reg 2 vm') `shouldBe` 1

            it "interprets Or to be False" $ do
                let vm  = makeVm [MoveI 0 0, MoveI 1 0, Or 2 0 1] []
                    vm' = run vm
                (VM.reg 2 vm') `shouldBe` 0

            it "interprets And to be True" $ do
                let vm  = makeVm [MoveI 0 1, MoveI 1 1, And 2 0 1] []
                    vm' = run vm
                (VM.reg 2 vm') `shouldBe` 1

            it "interprets And to be False" $ do
                let vm  = makeVm [MoveI 0 0, MoveI 1 1, And 2 0 1] []
                    vm' = run vm
                (VM.reg 2 vm') `shouldBe` 0

            it "interprets Not to be True" $ do
                let vm  = makeVm [MoveI 0 1, Not 1 0] []
                    vm' = run vm
                (VM.reg 1 vm') `shouldBe` 0

            it "interprets Not to be False" $ do
                let vm  = makeVm [MoveI 0 0, Not 1 0] []
                    vm' = run vm
                (VM.reg 1 vm') `shouldBe` 1

        context "branch instructions" $ do
            it "interprets B" $ do
                -- Branch should cause MoveI instruction to be skipped.
                let vm  = makeVm [B 2, MoveI 0 5] []
                    vm' = run vm
                (VM.reg 0 vm') `shouldBe` 0

            it "interprets BT and takes branch" $ do
                -- BT instruction should cause MoveI to be skipped.
                let vm  = makeVm [MoveI 0 1, BT 0 3, MoveI 1 5] []
                    vm' = run vm
                (VM.reg 1 vm') `shouldBe` 0

            it "interprets BT and does not take branch" $ do
                let vm  = makeVm [MoveI 0 0, BT 0 2, MoveI 1 5] []
                    vm' = run vm
                (VM.reg 1 vm') `shouldBe` 5

            it "interprets Ret" $ do
                -- Ret instruction should cause MoveI to be skipped.
                let lrIdx = 8
                    vm    = makeVm [MoveI lrIdx 3, Ret, MoveI 0 5] []
                    vm'   = run vm
                (VM.reg 0 vm') `shouldBe` 0
