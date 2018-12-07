module CPUSpec (cpuSpec) where

import Test.Hspec
import Data.Maybe (fromJust)
import Instr
import State as St hiding (regVal)
import qualified State as St (regVal)
import qualified Mem as Mem
import Data.Int (Int32)
import CPU
import Types

regVal :: PhyReg -> State -> Res Val
regVal p st = fmap fromJust (St.regVal p st)

runVm :: [FInstr] -> [Int32] -> State
runVm instrs memCnts = run state' where
    state' = state { mem = Mem.fromList memCnts }
    state = St.emptyDefault (instrs ++ [sysCall])

cpuSpec :: Spec
cpuSpec = describe "execution" $ do
    context "normal execution" $ do
        context "memory" $ do
            it "interprets MoveI" $ do
                let vm = runVm [moveI 0 5] []
                regVal 0 vm `shouldBe` Res 5

            it "interprets Move" $ do
                let vm = runVm [moveI 0 6, move 1 0] []
                regVal 1 vm `shouldBe` Res 6

            it "interprets LoadIdx" $ do
                let vm  = runVm [moveI 0 1, loadIdx 1 0 2] [1, 2, 3, 4, 5]
                regVal 1 vm `shouldBe` Res 4

            it "interprets LoadBaseIdx" $ do
                let vm  = runVm [moveI 0 1, moveI 1 3, loadBaseIdx 2 0 1] [1, 2, 3, 4, 5]
                regVal 2 vm `shouldBe` Res 5

            it "interprets StoreIdx" $ do
                let vm  = runVm [moveI 0 7, moveI 1 2, storeIdx 0 1 2] [0, 0, 0, 0, 0]
                St.mem vm `shouldBe` Mem.fromList [0, 0, 0, 0, 7]

            it "interprets StoreBaseIdx" $ do
                let vm  = runVm [moveI 0 7, moveI 1 2, moveI 2 3, storeBaseIdx 0 1 2] [0, 0, 0, 0, 0, 0]
                St.mem vm `shouldBe` Mem.fromList [0, 0, 0, 0, 0, 7]

        context "ALU instructions" $ do
            it "interprets Add" $ do
                let vm  = runVm [moveI 0 1, moveI 1 2, add 2 0 1] []
                regVal 2 vm `shouldBe` Res 3

            it "interprets AddI" $ do
                let vm  = runVm [moveI 0 2, addI 1 0 10] []
                regVal 1 vm `shouldBe` Res 12

            it "interprets Sub" $ do
                let vm  = runVm [moveI 0 5, moveI 1 3, sub 2 0 1] []
                regVal 2 vm `shouldBe` Res 2

            it "interprets SubI" $ do
                let vm  = runVm [moveI 0 10, subI 1 0 3] []
                regVal 1 vm `shouldBe` Res 7

            it "interprets Mult" $ do
                let vm  = runVm [moveI 0 10, moveI 1 3, mult 2 0 1] []
                regVal 2 vm `shouldBe` Res 30

            it "interprets Div" $ do
                let vm = runVm [moveI 0 10, moveI 1 2, divI 2 0 1] []
                regVal 2 vm `shouldBe` Res 5

            it "interprets Div to produce integer" $ do
                let vm = runVm [moveI 0 7, moveI 1 2, divI 2 0 1] []
                regVal 2 vm `shouldBe` Res 3

            it "interprets Eq to be True" $ do
                let vm  = runVm [moveI 0 1, moveI 1 1, eq 2 0 1] []
                regVal 2 vm `shouldBe` Res 1

            it "interprets Eq to be False" $ do
                let vm  = runVm [moveI 0 1, moveI 1 2, eq 2 0 1] []
                regVal 2 vm `shouldBe` Res 0

            it "interprets Lt to be True" $ do
                let vm  = runVm [moveI 0 1, moveI 1 2, lt 2 0 1] []
                regVal 2 vm `shouldBe` Res 1

            it "interprets Lt to be False" $ do
                let vm  = runVm [moveI 0 3, moveI 1 2, lt 2 0 1] []
                regVal 2 vm `shouldBe` Res 0

            it "interprets Or to be True" $ do
                let vm  = runVm [moveI 0 0, moveI 1 1, orI 2 0 1] []
                regVal 2 vm `shouldBe` Res 1

            it "interprets Or to be False" $ do
                let vm  = runVm [moveI 0 0, moveI 1 0, orI 2 0 1] []
                regVal 2 vm `shouldBe` Res 0

            it "interprets And to be True" $ do
                let vm  = runVm [moveI 0 1, moveI 1 1, andI 2 0 1] []
                regVal 2 vm `shouldBe` Res 1

            it "interprets And to be False" $ do
                let vm  = runVm [moveI 0 0, moveI 1 1, andI 2 0 1] []
                regVal 2 vm `shouldBe` Res 0

            it "interprets Not to be True" $ do
                let vm  = runVm [moveI 0 1, notI 1 0] []
                regVal 1 vm `shouldBe` Res 0

            it "interprets Not to be False" $ do
                let vm  = runVm [moveI 0 0, notI 1 0] []
                regVal 1 vm `shouldBe` Res 1

        context "branch instructions" $ do
            it "interprets B" $ do
                -- Branch should cause MoveI instruction to be skipped.
                let vm  = runVm [b 1, moveI 0 5] []
                regVal 0 vm `shouldBe` Res 0

            it "interprets BT and takes branch" $ do
                -- BT instruction should cause MoveI to be skipped.
                let vm  = runVm [moveI 0 1, bt 0 2, moveI 1 5] []
                regVal 1 vm `shouldBe` Res 0

            it "interprets BT and does not take branch" $ do
                let vm  = runVm [moveI 0 0, bt 0 1, moveI 1 5] []
                regVal 1 vm `shouldBe` Res 5

            it "interprets BF and takes branch" $ do
                -- BT instruction should cause MoveI to be skipped.
                let vm  = runVm [moveI 0 0, bf 0 2, moveI 1 5] []
                regVal 1 vm `shouldBe` Res 0

            it "interprets BF and does not take branch" $ do
                let vm  = runVm [moveI 0 1, bf 0 1, moveI 1 5] []
                regVal 1 vm `shouldBe` Res 5

            it "interprets Ret" $ do
                -- Ret instruction should cause MoveI to be skipped.
                let lrIdx = 13
                    vm    = runVm [moveI lrIdx 2, ret, moveI 0 5] []
                regVal 0 vm `shouldBe` Res 0

        context "output instructions" $ do
            it "interprets Print" $ do
                let vm = runVm [moveI 0 10, printI 0] []
                St.output vm `shouldBe` "10"

            it "interprets PrintC" $ do
                let vm = runVm [moveI 0 65, printC 0] []
                St.output vm `shouldBe` "A"

            it "interprets PrintLn" $ do
                let vm  = runVm [printLn] []
                St.output vm `shouldBe` "\n"

        context "running example programs" $ do
            it "runs bubble-sort" $ do
                let vm = runVm bubbleSort (replicate 32 0)
                St.output vm `shouldBe` "491203\n134920"

bubbleSort :: [FInstr]
bubbleSort = [AL (AddI 12 12 6),AL (Move 0 12),AL (SubI 0 0 5),Mem (StoreIdx 0 14 0),Mem (LoadIdx 0 14 0),AL (MoveI 1 4),Mem (StoreIdx 1 0 0),AL (MoveI 1 9),Mem (StoreIdx 1 0 1),AL (MoveI 1 1),Mem (StoreIdx 1 0 2),AL (MoveI 1 20),Mem (StoreIdx 1 0 3),AL (MoveI 1 3),Mem (StoreIdx 1 0 4),Mem (LoadIdx 0 14 0),Mem (StoreIdx 14 12 0),Mem (StoreIdx 13 12 1),AL (AddI 12 12 2),AL (Move 14 12),AL (MoveI 13 23),Mem (StoreIdx 0 12 0),AL (AddI 12 12 1),Branch (B 115),AL (SubI 12 12 1),Mem (LoadIdx 13 12 (-1)),Mem (LoadIdx 14 12 (-2)),AL (SubI 12 12 2),Out PrintLn,Mem (LoadIdx 0 14 0),Mem (StoreIdx 14 12 0),Mem (StoreIdx 13 12 1),AL (AddI 12 12 2),AL (Move 14 12),AL (MoveI 13 37),Mem (StoreIdx 0 12 0),AL (AddI 12 12 1),Branch (B 56),AL (SubI 12 12 1),Mem (LoadIdx 13 12 (-1)),Mem (LoadIdx 14 12 (-2)),AL (SubI 12 12 2),Mem (LoadIdx 0 14 0),Mem (StoreIdx 14 12 0),Mem (StoreIdx 13 12 1),AL (AddI 12 12 2),AL (Move 14 12),AL (MoveI 13 50),Mem (StoreIdx 0 12 0),AL (AddI 12 12 1),Branch (B 115),AL (SubI 12 12 1),Mem (LoadIdx 13 12 (-1)),Mem (LoadIdx 14 12 (-2)),AL (SubI 12 12 2),AL (SubI 12 12 6),Branch SysCall,AL (AddI 12 12 3),AL (MoveI 0 0),Mem (StoreIdx 0 14 3),Mem (LoadIdx 0 14 3),AL (MoveI 1 4),AL (Lt 0 0 1),Branch (BF 0 113),AL (MoveI 1 0),Mem (StoreIdx 1 14 2),Mem (LoadIdx 1 14 2),AL (MoveI 2 4),AL (Lt 1 1 2),Branch (BF 1 105),Mem (LoadIdx 2 14 0),Mem (LoadIdx 4 14 2),AL (MoveI 5 1),AL (Add 4 4 5),Mem (LoadBaseIdx 2 2 4),Mem (LoadIdx 3 14 0),Mem (LoadIdx 4 14 2),Mem (LoadBaseIdx 3 3 4),AL (Lt 2 2 3),Branch (BF 2 97),Mem (LoadIdx 3 14 0),Mem (LoadIdx 4 14 2),Mem (LoadBaseIdx 3 3 4),Mem (StoreIdx 3 14 1),Mem (LoadIdx 3 14 0),Mem (LoadIdx 4 14 2),Mem (LoadIdx 5 14 0),Mem (LoadIdx 6 14 2),AL (MoveI 7 1),AL (Add 6 6 7),Mem (LoadBaseIdx 5 5 6),Mem (StoreBaseIdx 5 3 4),Mem (LoadIdx 3 14 0),Mem (LoadIdx 4 14 2),AL (MoveI 6 1),AL (Add 4 4 6),Mem (LoadIdx 5 14 1),Mem (StoreBaseIdx 5 3 4),Mem (LoadIdx 2 14 2),AL (MoveI 3 1),AL (Add 2 2 3),Mem (StoreIdx 2 14 2),Mem (LoadIdx 1 14 2),AL (MoveI 2 4),AL (Lt 1 1 2),Branch (BT 1 69),Mem (LoadIdx 1 14 3),AL (MoveI 2 1),AL (Add 1 1 2),Mem (StoreIdx 1 14 3),Mem (LoadIdx 0 14 3),AL (MoveI 1 4),AL (Lt 0 0 1),Branch (BT 0 63),AL (SubI 12 12 3),Branch Ret,AL (AddI 12 12 1),AL (MoveI 0 0),Mem (StoreIdx 0 14 1),Mem (LoadIdx 0 14 1),AL (MoveI 1 5),AL (Lt 0 0 1),Branch (BF 0 134),Mem (LoadIdx 1 14 0),Mem (LoadIdx 2 14 1),Mem (LoadBaseIdx 1 1 2),Out (Print 1),Mem (LoadIdx 1 14 1),AL (MoveI 2 1),AL (Add 1 1 2),Mem (StoreIdx 1 14 1),Mem (LoadIdx 0 14 1),AL (MoveI 1 5),AL (Lt 0 0 1),Branch (BT 0 122),AL (SubI 12 12 1),Branch Ret]
