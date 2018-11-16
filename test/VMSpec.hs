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

            it "interprets Mult" $ do
                let vm  = runVm [MoveI 0 10, MoveI 1 3, Mult 2 0 1] []
                VM.regVal 2 vm `shouldBe` VM 30

            it "interprets Div" $ do
                let vm = runVm [MoveI 0 10, MoveI 1 2, Div 2 0 1] []
                VM.regVal 2 vm `shouldBe` VM 5

            it "interprets Div to produce integer" $ do
                let vm = runVm [MoveI 0 7, MoveI 1 2, Div 2 0 1] []
                VM.regVal 2 vm `shouldBe` VM 3

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
                State.output vm `shouldBe` "10"

            it "interprets PrintLn" $ do
                let vm  = runVm [PrintLn] []
                State.output vm `shouldBe` "\n"

        context "running example programs" $ do
            it "runs bubble-sort" $ do
                let vm = runVm bubbleSort (replicate 32 0)
                State.output vm `shouldBe` "491203\n134920"

bubbleSort :: [Instr]
bubbleSort = [Move {r = 0, from = 13},MoveI {r = 1, val = 4},StoreIdx {r = 1, base = 13, offset = 0},AddI {r = 13, x = 13, i = 1},MoveI {r = 1, val = 9},StoreIdx {r = 1, base = 13, offset = 0},AddI {r = 13, x = 13, i = 1},MoveI {r = 1, val = 1},StoreIdx {r = 1, base = 13, offset = 0},AddI {r = 13, x = 13, i = 1},MoveI {r = 1, val = 20},StoreIdx {r = 1, base = 13, offset = 0},AddI {r = 13, x = 13, i = 1},MoveI {r = 1, val = 3},StoreIdx {r = 1, base = 13, offset = 0},AddI {r = 13, x = 13, i = 1},StoreIdx {r = 0, base = 13, offset = 0},AddI {r = 13, x = 13, i = 1},LoadIdx {r = 0, base = 15, offset = 5},StoreIdx {r = 15, base = 13, offset = 0},StoreIdx {r = 14, base = 13, offset = 1},AddI {r = 13, x = 13, i = 2},Move {r = 15, from = 13},MoveI {r = 14, val = 26},StoreIdx {r = 0, base = 13, offset = 0},AddI {r = 13, x = 13, i = 1},B {addr = 122},SubI {r = 13, x = 13, i = 1},LoadIdx {r = 14, base = 13, offset = -1},LoadIdx {r = 15, base = 13, offset = -2},SubI {r = 13, x = 13, i = 2},PrintLn,LoadIdx {r = 0, base = 15, offset = 5},StoreIdx {r = 15, base = 13, offset = 0},StoreIdx {r = 14, base = 13, offset = 1},AddI {r = 13, x = 13, i = 2},Move {r = 15, from = 13},MoveI {r = 14, val = 40},StoreIdx {r = 0, base = 13, offset = 0},AddI {r = 13, x = 13, i = 1},B {addr = 59},SubI {r = 13, x = 13, i = 1},LoadIdx {r = 14, base = 13, offset = -1},LoadIdx {r = 15, base = 13, offset = -2},SubI {r = 13, x = 13, i = 2},LoadIdx {r = 0, base = 15, offset = 5},StoreIdx {r = 15, base = 13, offset = 0},StoreIdx {r = 14, base = 13, offset = 1},AddI {r = 13, x = 13, i = 2},Move {r = 15, from = 13},MoveI {r = 14, val = 53},StoreIdx {r = 0, base = 13, offset = 0},AddI {r = 13, x = 13, i = 1},B {addr = 122},SubI {r = 13, x = 13, i = 1},LoadIdx {r = 14, base = 13, offset = -1},LoadIdx {r = 15, base = 13, offset = -2},SubI {r = 13, x = 13, i = 2},SubI {r = 13, x = 13, i = 6},SysCall,MoveI {r = 0, val = 0},StoreIdx {r = 0, base = 13, offset = 0},AddI {r = 13, x = 13, i = 1},LoadIdx {r = 0, base = 15, offset = 1},MoveI {r = 1, val = 4},Lt {r = 0, x = 0, y = 1},BF {r = 0, addr = 120},MoveI {r = 1, val = 0},StoreIdx {r = 1, base = 13, offset = 0},AddI {r = 13, x = 13, i = 1},LoadIdx {r = 1, base = 15, offset = 2},MoveI {r = 2, val = 4},Lt {r = 1, x = 1, y = 2},BF {r = 1, addr = 111},LoadIdx {r = 2, base = 15, offset = 0},LoadIdx {r = 4, base = 15, offset = 2},MoveI {r = 5, val = 1},Add {r = 4, x = 4, y = 5},LoadBaseIdx {r = 2, base = 2, rOffset = 4},LoadIdx {r = 3, base = 15, offset = 0},LoadIdx {r = 4, base = 15, offset = 2},LoadBaseIdx {r = 3, base = 3, rOffset = 4},Lt {r = 2, x = 2, y = 3},BF {r = 2, addr = 103},LoadIdx {r = 3, base = 15, offset = 0},LoadIdx {r = 4, base = 15, offset = 2},LoadBaseIdx {r = 3, base = 3, rOffset = 4},StoreIdx {r = 3, base = 13, offset = 0},AddI {r = 13, x = 13, i = 1},LoadIdx {r = 3, base = 15, offset = 0},LoadIdx {r = 4, base = 15, offset = 2},LoadIdx {r = 5, base = 15, offset = 0},LoadIdx {r = 6, base = 15, offset = 2},MoveI {r = 7, val = 1},Add {r = 6, x = 6, y = 7},LoadBaseIdx {r = 5, base = 5, rOffset = 6},StoreBaseIdx {r = 5, base = 3, rOffset = 4},LoadIdx {r = 3, base = 15, offset = 0},LoadIdx {r = 4, base = 15, offset = 2},MoveI {r = 6, val = 1},Add {r = 4, x = 4, y = 6},LoadIdx {r = 5, base = 15, offset = 3},StoreBaseIdx {r = 5, base = 3, rOffset = 4},SubI {r = 13, x = 13, i = 1},LoadIdx {r = 2, base = 15, offset = 2},MoveI {r = 3, val = 1},Add {r = 2, x = 2, y = 3},StoreIdx {r = 2, base = 15, offset = 2},LoadIdx {r = 1, base = 15, offset = 2},MoveI {r = 2, val = 4},Lt {r = 1, x = 1, y = 2},BT {r = 1, addr = 73},LoadIdx {r = 1, base = 15, offset = 1},MoveI {r = 2, val = 1},Add {r = 1, x = 1, y = 2},StoreIdx {r = 1, base = 15, offset = 1},SubI {r = 13, x = 13, i = 1},LoadIdx {r = 0, base = 15, offset = 1},MoveI {r = 1, val = 4},Lt {r = 0, x = 0, y = 1},BT {r = 0, addr = 66},SubI {r = 13, x = 13, i = 1},Ret,MoveI {r = 0, val = 0},StoreIdx {r = 0, base = 13, offset = 0},AddI {r = 13, x = 13, i = 1},LoadIdx {r = 0, base = 15, offset = 1},MoveI {r = 1, val = 5},Lt {r = 0, x = 0, y = 1},BF {r = 0, addr = 141},LoadIdx {r = 1, base = 15, offset = 0},LoadIdx {r = 2, base = 15, offset = 1},LoadBaseIdx {r = 1, base = 1, rOffset = 2},Print {r = 1},LoadIdx {r = 1, base = 15, offset = 1},MoveI {r = 2, val = 1},Add {r = 1, x = 1, y = 2},StoreIdx {r = 1, base = 15, offset = 1},LoadIdx {r = 0, base = 15, offset = 1},MoveI {r = 1, val = 5},Lt {r = 0, x = 0, y = 1},BT {r = 0, addr = 129},SubI {r = 13, x = 13, i = 1},Ret]
