module ExecSpec (execSpec) where

import Test.Hspec
import Instr
import State as St
import qualified Mem as Mem
import Data.Int (Int32)
import Exec

runVm :: [FInstr] -> [Int32] -> State
runVm instrs memCnts = run state' where
    state' = state { mem = Mem.fromList memCnts }
    state = St.emptyDefault (instrs ++ [SysCall])

execSpec :: Spec
execSpec = describe "execution" $ do
    context "normal execution" $ do
        context "memory" $ do
            it "interprets MoveI" $ do
                let vm = runVm [MoveI 0 5] []
                St.regVal 0 vm `shouldBe` Res 5

            it "interprets Move" $ do
                let vm = runVm [MoveI 0 6, Move 1 0] []
                St.regVal 1 vm `shouldBe` Res 6

            it "interprets LoadIdx" $ do
                let vm  = runVm [MoveI 0 1, LoadIdx 1 0 2] [1, 2, 3, 4, 5]
                St.regVal 1 vm `shouldBe` Res 4

            it "interprets LoadBaseIdx" $ do
                let vm  = runVm [MoveI 0 1, MoveI 1 3, LoadBaseIdx 2 0 1] [1, 2, 3, 4, 5]
                St.regVal 2 vm `shouldBe` Res 5

            it "interprets StoreIdx" $ do
                let vm  = runVm [MoveI 0 7, MoveI 1 2, StoreIdx 0 1 2] [0, 0, 0, 0, 0]
                St.mem vm `shouldBe` Mem.fromList [0, 0, 0, 0, 7]

            it "interprets StoreBaseIdx" $ do
                let vm  = runVm [MoveI 0 7, MoveI 1 2, MoveI 2 3, StoreBaseIdx 0 1 2] [0, 0, 0, 0, 0, 0]
                St.mem vm `shouldBe` Mem.fromList [0, 0, 0, 0, 0, 7]

        -- context "ALU instructions" $ do
        --     it "interprets Add" $ do
        --         let vm  = runVm [MoveI 0 1, MoveI 1 2, Add 2 0 1] []
        --         St.regVal 2 vm `shouldBe` Res 3
        --
        --     it "interprets AddI" $ do
        --         let vm  = runVm [MoveI 0 2, AddI 1 0 10] []
        --         St.regVal 1 vm `shouldBe` Res 12
        --
        --     it "interprets Sub" $ do
        --         let vm  = runVm [MoveI 0 5, MoveI 1 3, Sub 2 0 1] []
        --         St.regVal 2 vm `shouldBe` Res 2
        --
        --     it "interprets SubI" $ do
        --         let vm  = runVm [MoveI 0 10, SubI 1 0 3] []
        --         St.regVal 1 vm `shouldBe` Res 7
        --
        --     it "interprets Mult" $ do
        --         let vm  = runVm [MoveI 0 10, MoveI 1 3, Mult 2 0 1] []
        --         St.regVal 2 vm `shouldBe` Res 30
        --
        --     it "interprets Div" $ do
        --         let vm = runVm [MoveI 0 10, MoveI 1 2, Div 2 0 1] []
        --         St.regVal 2 vm `shouldBe` Res 5
        --
        --     it "interprets Div to produce integer" $ do
        --         let vm = runVm [MoveI 0 7, MoveI 1 2, Div 2 0 1] []
        --         St.regVal 2 vm `shouldBe` Res 3
        --
        --     it "interprets Eq to be True" $ do
        --         let vm  = runVm [MoveI 0 1, MoveI 1 1, Eq 2 0 1] []
        --         St.regVal 2 vm `shouldBe` Res 1
        --
        --     it "interprets Eq to be False" $ do
        --         let vm  = runVm [MoveI 0 1, MoveI 1 2, Eq 2 0 1] []
        --         St.regVal 2 vm `shouldBe` Res 0
        --
        --     it "interprets Lt to be True" $ do
        --         let vm  = runVm [MoveI 0 1, MoveI 1 2, Lt 2 0 1] []
        --         St.regVal 2 vm `shouldBe` Res 1
        --
        --     it "interprets Lt to be False" $ do
        --         let vm  = runVm [MoveI 0 3, MoveI 1 2, Lt 2 0 1] []
        --         St.regVal 2 vm `shouldBe` Res 0
        --
        --     it "interprets Or to be True" $ do
        --         let vm  = runVm [MoveI 0 0, MoveI 1 1, Or 2 0 1] []
        --         St.regVal 2 vm `shouldBe` Res 1
        --
        --     it "interprets Or to be False" $ do
        --         let vm  = runVm [MoveI 0 0, MoveI 1 0, Or 2 0 1] []
        --         St.regVal 2 vm `shouldBe` Res 0
        --
        --     it "interprets And to be True" $ do
        --         let vm  = runVm [MoveI 0 1, MoveI 1 1, And 2 0 1] []
        --         St.regVal 2 vm `shouldBe` Res 1
        --
        --     it "interprets And to be False" $ do
        --         let vm  = runVm [MoveI 0 0, MoveI 1 1, And 2 0 1] []
        --         St.regVal 2 vm `shouldBe` Res 0
        --
        --     it "interprets Not to be True" $ do
        --         let vm  = runVm [MoveI 0 1, Not 1 0] []
        --         St.regVal 1 vm `shouldBe` Res 0
        --
        --     it "interprets Not to be False" $ do
        --         let vm  = runVm [MoveI 0 0, Not 1 0] []
        --         St.regVal 1 vm `shouldBe` Res 1
        --
        -- context "branch instructions" $ do
        --     it "interprets B" $ do
        --         -- Branch should cause MoveI instruction to be skipped.
        --         let vm  = runVm [B 1, MoveI 0 5] []
        --         St.regVal 0 vm `shouldBe` Res 0
        --
        --     it "interprets BT and takes branch" $ do
        --         -- BT instruction should cause MoveI to be skipped.
        --         let vm  = runVm [MoveI 0 1, BT 0 2, MoveI 1 5] []
        --         St.regVal 1 vm `shouldBe` Res 0
        --
        --     it "interprets BT and does not take branch" $ do
        --         let vm  = runVm [MoveI 0 0, BT 0 1, MoveI 1 5] []
        --         St.regVal 1 vm `shouldBe` Res 5
        --
        --     it "interprets BF and takes branch" $ do
        --         -- BT instruction should cause MoveI to be skipped.
        --         let vm  = runVm [MoveI 0 0, BF 0 2, MoveI 1 5] []
        --         St.regVal 1 vm `shouldBe` Res 0
        --
        --     it "interprets BF and does not take branch" $ do
        --         let vm  = runVm [MoveI 0 1, BF 0 1, MoveI 1 5] []
        --         St.regVal 1 vm `shouldBe` Res 5
        --
        --     it "interprets Ret" $ do
        --         -- Ret instruction should cause MoveI to be skipped.
        --         let lrIdx = 13
        --             vm    = runVm [MoveI lrIdx 2, Ret, MoveI 0 5] []
        --         St.regVal 0 vm `shouldBe` Res 0
        --
        -- context "output instructions" $ do
        --     it "interprets Print" $ do
        --         let vm  = runVm [MoveI 0 10, Print 0] []
        --         St.output vm `shouldBe` "10"
        --
        --     it "interprets PrintLn" $ do
        --         let vm  = runVm [PrintLn] []
        --         St.output vm `shouldBe` "\n"
        --
        -- context "running example programs" $ do
        --     it "runs bubble-sort" $ do
        --         let vm = runVm bubbleSort (replicate 32 0)
        --         St.output vm `shouldBe` "491203\n134920"

-- bubbleSort :: [FInstr]
-- bubbleSort = [AddI {r = 12, x = 12, i = 11},Move {r = 0, from = 12},SubI {r = 0, x = 0, i = 5},StoreIdx {r = 0, base = 14, offset = 5},LoadIdx {r = 0, base = 14, offset = 5},MoveI {r = 1, val = 4},StoreIdx {r = 1, base = 0, offset = 0},MoveI {r = 1, val = 9},StoreIdx {r = 1, base = 0, offset = 1},MoveI {r = 1, val = 1},StoreIdx {r = 1, base = 0, offset = 2},MoveI {r = 1, val = 20},StoreIdx {r = 1, base = 0, offset = 3},MoveI {r = 1, val = 3},StoreIdx {r = 1, base = 0, offset = 4},MoveI {r = 0, val = 0},StoreIdx {r = 0, base = 14, offset = 4},LoadIdx {r = 0, base = 14, offset = 4},MoveI {r = 1, val = 5},Lt {r = 0, x = 0, y = 1},BF {r = 0, addr = 32},LoadIdx {r = 1, base = 14, offset = 5},LoadIdx {r = 2, base = 14, offset = 4},LoadBaseIdx {r = 1, base = 1, rOffset = 2},Print {r = 1},LoadIdx {r = 1, base = 14, offset = 4},MoveI {r = 2, val = 1},Add {r = 1, x = 1, y = 2},StoreIdx {r = 1, base = 14, offset = 4},LoadIdx {r = 0, base = 14, offset = 4},MoveI {r = 1, val = 5},Lt {r = 0, x = 0, y = 1},BT {r = 0, addr = 20},PrintLn,MoveI {r = 0, val = 0},StoreIdx {r = 0, base = 14, offset = 4},LoadIdx {r = 0, base = 14, offset = 4},MoveI {r = 1, val = 4},Lt {r = 0, x = 0, y = 1},BF {r = 0, addr = 89},MoveI {r = 1, val = 0},StoreIdx {r = 1, base = 14, offset = 2},LoadIdx {r = 1, base = 14, offset = 2},MoveI {r = 2, val = 4},Lt {r = 1, x = 1, y = 2},BF {r = 1, addr = 81},LoadIdx {r = 2, base = 14, offset = 5},LoadIdx {r = 4, base = 14, offset = 2},MoveI {r = 5, val = 1},Add {r = 4, x = 4, y = 5},LoadBaseIdx {r = 2, base = 2, rOffset = 4},LoadIdx {r = 3, base = 14, offset = 5},LoadIdx {r = 4, base = 14, offset = 2},LoadBaseIdx {r = 3, base = 3, rOffset = 4},Lt {r = 2, x = 2, y = 3},BF {r = 2, addr = 73},LoadIdx {r = 3, base = 14, offset = 5},LoadIdx {r = 4, base = 14, offset = 2},LoadBaseIdx {r = 3, base = 3, rOffset = 4},StoreIdx {r = 3, base = 14, offset = 1},LoadIdx {r = 3, base = 14, offset = 5},LoadIdx {r = 4, base = 14, offset = 2},LoadIdx {r = 5, base = 14, offset = 5},LoadIdx {r = 6, base = 14, offset = 2},MoveI {r = 7, val = 1},Add {r = 6, x = 6, y = 7},LoadBaseIdx {r = 5, base = 5, rOffset = 6},StoreBaseIdx {r = 5, base = 3, rOffset = 4},LoadIdx {r = 3, base = 14, offset = 5},LoadIdx {r = 4, base = 14, offset = 2},MoveI {r = 6, val = 1},Add {r = 4, x = 4, y = 6},LoadIdx {r = 5, base = 14, offset = 1},StoreBaseIdx {r = 5, base = 3, rOffset = 4},LoadIdx {r = 2, base = 14, offset = 2},MoveI {r = 3, val = 1},Add {r = 2, x = 2, y = 3},StoreIdx {r = 2, base = 14, offset = 2},LoadIdx {r = 1, base = 14, offset = 2},MoveI {r = 2, val = 4},Lt {r = 1, x = 1, y = 2},BT {r = 1, addr = 45},LoadIdx {r = 1, base = 14, offset = 4},MoveI {r = 2, val = 1},Add {r = 1, x = 1, y = 2},StoreIdx {r = 1, base = 14, offset = 4},LoadIdx {r = 0, base = 14, offset = 4},MoveI {r = 1, val = 4},Lt {r = 0, x = 0, y = 1},BT {r = 0, addr = 39},MoveI {r = 0, val = 0},StoreIdx {r = 0, base = 14, offset = 4},LoadIdx {r = 0, base = 14, offset = 4},MoveI {r = 1, val = 5},Lt {r = 0, x = 0, y = 1},BF {r = 0, addr = 107},LoadIdx {r = 1, base = 14, offset = 5},LoadIdx {r = 2, base = 14, offset = 4},LoadBaseIdx {r = 1, base = 1, rOffset = 2},Print {r = 1},LoadIdx {r = 1, base = 14, offset = 4},MoveI {r = 2, val = 1},Add {r = 1, x = 1, y = 2},StoreIdx {r = 1, base = 14, offset = 4},LoadIdx {r = 0, base = 14, offset = 4},MoveI {r = 1, val = 5},Lt {r = 0, x = 0, y = 1},BT {r = 0, addr = 95},SubI {r = 12, x = 12, i = 11},SysCall]
