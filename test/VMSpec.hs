module VMSpec (vmSpec) where

import Test.Hspec
import Instr
import State as State
import VM as VM
import qualified Mem as Mem
import Bypass as BP
import Data.Int (Int32)
import Debug.Trace

runVm :: [Instr] -> [Int32] -> State
runVm instrs memCnts = run state' where
    state' = state { mem = Mem.fromList memCnts }
    state = State.emptyDefault (instrs ++ [SysCall])

vmSpec :: Spec
vmSpec = describe "vm" $ do
    context "normal execution" $ do
        context "memory" $ do
            it "interprets MoveI" $ do
                let vm = runVm [MoveI 1 5] []
                VM.regVal 1 vm `shouldBe` VM 5

            it "interprets Move" $ do
                let vm = runVm [MoveI 0 6, Move 1 0] []
                VM.regVal 1 vm `shouldBe` VM 6

            it "interprets LoadIdx" $ do
                let vm  = runVm [MoveI 0 1, LoadIdx 1 0 2] [1, 2, 3, 4, 5]
                trace (show vm) $ VM.regVal 1 vm `shouldBe` VM 4

            it "interprets LoadBaseIdx" $ do
                let vm  = runVm [MoveI 0 1, MoveI 1 3, LoadBaseIdx 2 0 1] [1, 2, 3, 4, 5]
                VM.regVal 2 vm `shouldBe` VM 5

            it "interprets StoreIdx" $ do
                let vm  = runVm [MoveI 0 7, MoveI 1 2, StoreIdx 0 1 2] [0, 0, 0, 0, 0]
                State.mem vm `shouldBe` Mem.fromList [0, 0, 0, 0, 7]

            it "interprets StoreBaseIdx" $ do
                let vm  = runVm [MoveI 0 7, MoveI 1 2, MoveI 2 3, StoreBaseIdx 0 1 2] [0, 0, 0, 0, 0, 0]
                State.mem vm `shouldBe` Mem.fromList [0, 0, 0, 0, 0, 7]

        context "ALU instructions" $ do
            it "interprets Add" $ do
                let vm  = runVm [MoveI 0 1, MoveI 1 2, Add 2 0 1] []
                VM.regVal 2 vm `shouldBe` VM 3

            it "interprets AddI" $ do
                let vm  = runVm [MoveI 0 2, AddI 1 0 10] []
                VM.regVal 1 vm `shouldBe` VM 12

            it "interprets Sub" $ do
                let vm  = runVm [MoveI 0 5, MoveI 1 3, Sub 2 0 1] []
                VM.regVal 2 vm `shouldBe` VM 2

            it "interprets SubI" $ do
                let vm  = runVm [MoveI 0 10, SubI 1 0 3] []
                VM.regVal 1 vm `shouldBe` VM 7

            it "interprets MultI" $ do
                let vm  = runVm [MoveI 0 10, MoveI 1 3, Mult 2 0 1] []
                VM.regVal 2 vm `shouldBe` VM 30

            it "interprets Eq to be True" $ do
                let vm  = runVm [MoveI 0 1, MoveI 1 1, Eq 2 0 1] []
                VM.regVal 2 vm `shouldBe` VM 1

            it "interprets Eq to be False" $ do
                let vm  = runVm [MoveI 0 1, MoveI 1 2, Eq 2 0 1] []
                VM.regVal 2 vm `shouldBe` VM 0

            it "interprets Lt to be True" $ do
                let vm  = runVm [MoveI 0 1, MoveI 1 2, Lt 2 0 1] []
                VM.regVal 2 vm `shouldBe` VM 1

            it "interprets Lt to be False" $ do
                let vm  = runVm [MoveI 0 3, MoveI 1 2, Lt 2 0 1] []
                VM.regVal 2 vm `shouldBe` VM 0

            it "interprets Or to be True" $ do
                let vm  = runVm [MoveI 0 0, MoveI 1 1, Or 2 0 1] []
                VM.regVal 2 vm `shouldBe` VM 1

            it "interprets Or to be False" $ do
                let vm  = runVm [MoveI 0 0, MoveI 1 0, Or 2 0 1] []
                VM.regVal 2 vm `shouldBe` VM 0

            it "interprets And to be True" $ do
                let vm  = runVm [MoveI 0 1, MoveI 1 1, And 2 0 1] []
                VM.regVal 2 vm `shouldBe` VM 1

            it "interprets And to be False" $ do
                let vm  = runVm [MoveI 0 0, MoveI 1 1, And 2 0 1] []
                VM.regVal 2 vm `shouldBe` VM 0

            it "interprets Not to be True" $ do
                let vm  = runVm [MoveI 0 1, Not 1 0] []
                VM.regVal 1 vm `shouldBe` VM 0

            it "interprets Not to be False" $ do
                let vm  = runVm [MoveI 0 0, Not 1 0] []
                VM.regVal 1 vm `shouldBe` VM 1

        context "branch instructions" $ do
            it "interprets B" $ do
                -- Branch should cause MoveI instruction to be skipped.
                let vm  = runVm [B 1, MoveI 0 5] []
                VM.regVal 0 vm `shouldBe` VM 0

            it "interprets BT and takes branch" $ do
                -- BT instruction should cause MoveI to be skipped.
                let vm  = runVm [MoveI 0 1, BT 0 2, MoveI 1 5] []
                VM.regVal 1 vm `shouldBe` VM 0

            it "interprets BT and does not take branch" $ do
                let vm  = runVm [MoveI 0 0, BT 0 1, MoveI 1 5] []
                VM.regVal 1 vm `shouldBe` VM 5

            it "interprets BF and takes branch" $ do
                -- BT instruction should cause MoveI to be skipped.
                let vm  = runVm [MoveI 0 0, BF 0 2, MoveI 1 5] []
                VM.regVal 1 vm `shouldBe` VM 0

            it "interprets BF and does not take branch" $ do
                let vm  = runVm [MoveI 0 1, BF 0 1, MoveI 1 5] []
                VM.regVal 1 vm `shouldBe` VM 5

            it "interprets Ret" $ do
                -- Ret instruction should cause MoveI to be skipped.
                let lrIdx = 14
                    vm    = runVm [MoveI lrIdx 2, Ret, MoveI 0 5] []
                VM.regVal 0 vm `shouldBe` VM 0

        context "output instructions" $ do
            it "interprets Print" $ do
                let vm  = runVm [MoveI 0 10, Print 0] []
                State.output vm `shouldBe` "10\n"

            it "interprets PrintLn" $ do
                let vm  = runVm [PrintLn] []
                State.output vm `shouldBe` "\n"

        context "running example programs" $ do
            it "runs bubble-sort" $ do
                pending

bubbleSort :: [Instr]
bubbleSort = []
