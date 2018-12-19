module CPUSpec (cpuSpec) where

import Test.Hspec
import Instr
import State as St hiding (newestRegVal)
import qualified State as St (newestRegVal)
import qualified Mem as Mem
import Data.Int (Int32)
import CPU
import Types
import Debug.Trace

regVal :: RegIdx -> State -> Res Val
regVal r st = trace ("CHECK: " ++ debugShow st ++ "\n//////\n") $ do
    phy  <- St.getPhyReg r st
    mVal <- St.newestRegVal phy st
    case mVal of
        Nothing  -> error "Test reg val had no value"
        Just val -> return val

runVm :: [FInstr] -> [Int32] -> State
runVm instrs memCnts = run state' where
    state' = state { mem = Mem.fromList memCnts }
    state = St.emptyDefault (instrs ++ [sysCall])

cpuSpec :: Spec
cpuSpec = describe "execution" $ do
    context "normal execution" $ do
        context "memory" $ do
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

            it "handles RAW hazards" $ do
                let vm  = runVm [moveI 0 0, moveI 1 5, storeIdx 1 0 0, loadIdx 2 0 0] [0]
                regVal 2 vm `shouldBe` Res 5

            it "continues execution after RAW hazard" $ do
                let vm  = runVm [moveI 0 0, moveI 1 5, storeIdx 1 0 0, loadIdx 2 0 0, moveI 3 10] [0]
                regVal 3 vm `shouldBe` Res 10

        context "ALU instructions" $ do
            it "interprets MoveI" $ do
                let vm = runVm [moveI 0 5] []
                regVal 0 vm `shouldBe` Res 5

            it "interprets Move" $ do
                let vm = runVm [moveI 0 6, move 1 0] []
                regVal 1 vm `shouldBe` Res 6

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
                let vm  = runVm [b 1, printLn] []
                output vm `shouldBe` ""

            it "interprets BT and takes branch" $ do
                -- BT instruction should cause MoveI to be skipped.
                let vm  = runVm [moveI 0 1, bt 0 2, printLn] []
                output vm `shouldBe` ""

            it "interprets BT and does not take branch" $ do
                let vm  = runVm [moveI 0 0, bt 0 1, printLn] []
                output vm `shouldBe` "\n"

            it "interprets BF and takes branch" $ do
                -- BT instruction should cause MoveI to be skipped.
                let vm  = runVm [moveI 0 0, bf 0 2, printLn] []
                output vm `shouldBe` ""

            it "interprets BF and does not take branch" $ do
                let vm  = runVm [moveI 0 1, bf 0 1, printLn] []
                output vm `shouldBe` "\n"

            it "interprets Ret" $ do
                -- Ret instruction should cause MoveI to be skipped.
                let lrIdx = 13
                    vm    = runVm [moveI lrIdx 2, ret (), printLn] []
                output vm `shouldBe` ""

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

        context "updating special registers" $ do
            it "resolves read after write hazards" $ do
                let vm = runVm [addI 12 12 6, move 0 12] []
                regVal 0 vm `shouldBe` Res 6

        context "running example programs" $ do
            it "runs few additions" $ do
                let vm = runVm fewAdditions (replicate 16 0)
                St.output vm `shouldBe` "6"

            it "run additions" $ do
                let vm = runVm additions (replicate 16 0)
                St.output vm `shouldBe` "28"

            it "runs many additions" $ do
                let vm = runVm manyAdditions (replicate 16 0)
                St.output vm `shouldBe` "55"

            it "runs function return" $ do
                let vm = runVm funcRet (replicate 16 0)
                St.output vm `shouldBe` "11"

            it "runs bubble-sort" $ do
                let vm = runVm bubbleSort (replicate 32 0)
                St.output vm `shouldBe` "491203\n134920"

            it "runs brainfuck interpreter" $ do
                let vm = runVm bfInter (replicate 64 0)
                St.output vm `shouldBe` "0.0.4.0.0.0.0.0."

fewAdditions :: [FInstr]
fewAdditions = [AL (MoveI 0 1),AL (MoveI 2 2),AL (Add 0 0 2),AL (MoveI 1 3),AL (Add 0 0 1),Out (Print 0),Branch SysCall]

additions :: [FInstr]
additions = [AL (MoveI 0 1),AL (MoveI 6 2),AL (Add 0 0 6),AL (MoveI 5 3),AL (Add 0 0 5),AL (MoveI 4 4),AL (Add 0 0 4),AL (MoveI 3 5),AL (Add 0 0 3),AL (MoveI 2 6),AL (Add 0 0 2),AL (MoveI 1 7),AL (Add 0 0 1),Out (Print 0),Branch SysCall]

manyAdditions :: [FInstr]
manyAdditions = [AL (MoveI 0 1),AL (MoveI 9 2),AL (Add 0 0 9),AL (MoveI 8 3),AL (Add 0 0 8),AL (MoveI 7 4),AL (Add 0 0 7),AL (MoveI 6 5),AL (Add 0 0 6),AL (MoveI 5 6),AL (Add 0 0 5),AL (MoveI 4 7),AL (Add 0 0 4),AL (MoveI 3 8),AL (Add 0 0 3),AL (MoveI 2 9),AL (Add 0 0 2),AL (MoveI 1 10),AL (Add 0 0 1),Out (Print 0),Branch SysCall]

funcRet :: [FInstr]
funcRet = [AL (AddI 12 12 1),Mem (StoreIdx 14 12 0),Mem (StoreIdx 13 12 1),AL (AddI 12 12 2),AL (Move 14 12),AL (MoveI 13 6),Branch (B 15),Mem (LoadIdx 13 12 (-1)),Mem (LoadIdx 14 12 (-2)),AL (SubI 12 12 2),AL (Move 0 15),Mem (StoreIdx 0 14 0),Mem (LoadIdx 0 14 0),Out (Print 0),AL (SubI 12 12 1),Branch SysCall,AL (MoveI 15 10),AL (MoveI 0 1),AL (Add 15 15 0),Branch (Ret ())]

bubbleSort :: [FInstr]
bubbleSort = [AL (AddI 12 12 6),AL (Move 0 12),AL (SubI 0 0 5),Mem (StoreIdx 0 14 0),Mem (LoadIdx 0 14 0),AL (MoveI 1 4),Mem (StoreIdx 1 0 0),AL (MoveI 1 9),Mem (StoreIdx 1 0 1),AL (MoveI 1 1),Mem (StoreIdx 1 0 2),AL (MoveI 1 20),Mem (StoreIdx 1 0 3),AL (MoveI 1 3),Mem (StoreIdx 1 0 4),Mem (LoadIdx 0 14 0),Mem (StoreIdx 14 12 0),Mem (StoreIdx 13 12 1),AL (AddI 12 12 2),AL (Move 14 12),AL (MoveI 13 23),Mem (StoreIdx 0 12 0),AL (AddI 12 12 1),Branch (B 115),AL (SubI 12 12 1),Mem (LoadIdx 13 12 (-1)),Mem (LoadIdx 14 12 (-2)),AL (SubI 12 12 2),Out PrintLn,Mem (LoadIdx 0 14 0),Mem (StoreIdx 14 12 0),Mem (StoreIdx 13 12 1),AL (AddI 12 12 2),AL (Move 14 12),AL (MoveI 13 37),Mem (StoreIdx 0 12 0),AL (AddI 12 12 1),Branch (B 56),AL (SubI 12 12 1),Mem (LoadIdx 13 12 (-1)),Mem (LoadIdx 14 12 (-2)),AL (SubI 12 12 2),Mem (LoadIdx 0 14 0),Mem (StoreIdx 14 12 0),Mem (StoreIdx 13 12 1),AL (AddI 12 12 2),AL (Move 14 12),AL (MoveI 13 50),Mem (StoreIdx 0 12 0),AL (AddI 12 12 1),Branch (B 115),AL (SubI 12 12 1),Mem (LoadIdx 13 12 (-1)),Mem (LoadIdx 14 12 (-2)),AL (SubI 12 12 2),AL (SubI 12 12 6),Branch SysCall,AL (AddI 12 12 3),AL (MoveI 0 0),Mem (StoreIdx 0 14 3),Mem (LoadIdx 0 14 3),AL (MoveI 1 4),AL (Lt 0 0 1),Branch (BF 0 113),AL (MoveI 1 0),Mem (StoreIdx 1 14 2),Mem (LoadIdx 1 14 2),AL (MoveI 2 4),AL (Lt 1 1 2),Branch (BF 1 105),Mem (LoadIdx 2 14 0),Mem (LoadIdx 4 14 2),AL (MoveI 5 1),AL (Add 4 4 5),Mem (LoadBaseIdx 2 2 4),Mem (LoadIdx 3 14 0),Mem (LoadIdx 4 14 2),Mem (LoadBaseIdx 3 3 4),AL (Lt 2 2 3),Branch (BF 2 97),Mem (LoadIdx 3 14 0),Mem (LoadIdx 4 14 2),Mem (LoadBaseIdx 3 3 4),Mem (StoreIdx 3 14 1),Mem (LoadIdx 3 14 0),Mem (LoadIdx 4 14 2),Mem (LoadIdx 5 14 0),Mem (LoadIdx 6 14 2),AL (MoveI 7 1),AL (Add 6 6 7),Mem (LoadBaseIdx 5 5 6),Mem (StoreBaseIdx 5 3 4),Mem (LoadIdx 3 14 0),Mem (LoadIdx 4 14 2),AL (MoveI 6 1),AL (Add 4 4 6),Mem (LoadIdx 5 14 1),Mem (StoreBaseIdx 5 3 4),Mem (LoadIdx 2 14 2),AL (MoveI 3 1),AL (Add 2 2 3),Mem (StoreIdx 2 14 2),Mem (LoadIdx 1 14 2),AL (MoveI 2 4),AL (Lt 1 1 2),Branch (BT 1 69),Mem (LoadIdx 1 14 3),AL (MoveI 2 1),AL (Add 1 1 2),Mem (StoreIdx 1 14 3),Mem (LoadIdx 0 14 3),AL (MoveI 1 4),AL (Lt 0 0 1),Branch (BT 0 63),AL (SubI 12 12 3),ret (),AL (AddI 12 12 1),AL (MoveI 0 0),Mem (StoreIdx 0 14 1),Mem (LoadIdx 0 14 1),AL (MoveI 1 5),AL (Lt 0 0 1),Branch (BF 0 134),Mem (LoadIdx 1 14 0),Mem (LoadIdx 2 14 1),Mem (LoadBaseIdx 1 1 2),Out (Print 1),Mem (LoadIdx 1 14 1),AL (MoveI 2 1),AL (Add 1 1 2),Mem (StoreIdx 1 14 1),Mem (LoadIdx 0 14 1),AL (MoveI 1 5),AL (Lt 0 0 1),Branch (BT 0 122),AL (SubI 12 12 1),ret ()]

bfInter :: [FInstr]
bfInter = [AL (AddI 12 12 30),AL (Move 0 12),AL (SubI 0 0 20),Mem (StoreIdx 0 14 9),AL (Move 0 12),AL (SubI 0 0 29),Mem (StoreIdx 0 14 0),Mem (LoadIdx 0 14 9),AL (MoveI 1 43),Mem (StoreIdx 1 0 0),AL (MoveI 1 43),Mem (StoreIdx 1 0 1),AL (MoveI 1 43),Mem (StoreIdx 1 0 2),AL (MoveI 1 43),Mem (StoreIdx 1 0 3),AL (MoveI 1 62),Mem (StoreIdx 1 0 4),AL (MoveI 1 62),Mem (StoreIdx 1 0 5),AL (MoveI 1 91),Mem (StoreIdx 1 0 6),AL (MoveI 1 45),Mem (StoreIdx 1 0 7),AL (MoveI 1 93),Mem (StoreIdx 1 0 8),AL (MoveI 1 60),Mem (StoreIdx 1 0 9),AL (MoveI 1 60),Mem (StoreIdx 1 0 10),AL (MoveI 1 91),Mem (StoreIdx 1 0 11),AL (MoveI 1 45),Mem (StoreIdx 1 0 12),AL (MoveI 1 62),Mem (StoreIdx 1 0 13),AL (MoveI 1 62),Mem (StoreIdx 1 0 14),AL (MoveI 1 43),Mem (StoreIdx 1 0 15),AL (MoveI 1 60),Mem (StoreIdx 1 0 16),AL (MoveI 1 60),Mem (StoreIdx 1 0 17),AL (MoveI 1 93),Mem (StoreIdx 1 0 18),AL (MoveI 1 0),Mem (StoreIdx 1 0 19),Mem (LoadIdx 0 14 0),AL (MoveI 1 0),Mem (StoreIdx 1 0 0),AL (MoveI 1 0),Mem (StoreIdx 1 0 1),AL (MoveI 1 0),Mem (StoreIdx 1 0 2),AL (MoveI 1 0),Mem (StoreIdx 1 0 3),AL (MoveI 1 0),Mem (StoreIdx 1 0 4),AL (MoveI 1 0),Mem (StoreIdx 1 0 5),AL (MoveI 1 0),Mem (StoreIdx 1 0 6),AL (MoveI 1 0),Mem (StoreIdx 1 0 7),Mem (LoadIdx 0 14 0),AL (MoveI 1 8),Mem (LoadIdx 2 14 9),Mem (StoreIdx 14 12 0),Mem (StoreIdx 13 12 1),AL (AddI 12 12 2),AL (Move 14 12),AL (MoveI 13 77),Mem (StoreIdx 0 12 0),Mem (StoreIdx 1 12 1),Mem (StoreIdx 2 12 2),AL (AddI 12 12 3),Branch (B 98),AL (SubI 12 12 3),Mem (LoadIdx 13 12 (-1)),Mem (LoadIdx 14 12 (-2)),AL (SubI 12 12 2),Mem (LoadIdx 0 14 0),AL (MoveI 1 8),Mem (StoreIdx 14 12 0),Mem (StoreIdx 13 12 1),AL (AddI 12 12 2),AL (Move 14 12),AL (MoveI 13 92),Mem (StoreIdx 0 12 0),Mem (StoreIdx 1 12 1),AL (AddI 12 12 2),Branch (B 298),AL (SubI 12 12 2),Mem (LoadIdx 13 12 (-1)),Mem (LoadIdx 14 12 (-2)),AL (SubI 12 12 2),AL (SubI 12 12 30),Branch SysCall,AL (AddI 12 12 3),AL (MoveI 0 0),Mem (StoreIdx 0 14 5),AL (MoveI 0 0),Mem (StoreIdx 0 14 4),Mem (LoadIdx 0 14 2),Mem (LoadIdx 2 14 4),Mem (LoadBaseIdx 0 0 2),AL (MoveI 1 0),AL (Eq 0 0 1),AL (Not 0 0),Branch (BF 0 184),Mem (LoadIdx 1 14 2),Mem (LoadIdx 2 14 4),Mem (LoadBaseIdx 1 1 2),Mem (StoreIdx 1 14 3),Mem (LoadIdx 2 14 3),Mem (LoadIdx 3 14 4),Mem (LoadIdx 4 14 2),Mem (LoadIdx 5 14 0),Mem (LoadIdx 6 14 5),Mem (LoadBaseIdx 5 5 6),Mem (StoreIdx 14 12 0),Mem (StoreIdx 13 12 1),AL (AddI 12 12 2),AL (Move 14 12),AL (MoveI 13 131),Mem (StoreIdx 2 12 0),Mem (StoreIdx 3 12 1),Mem (StoreIdx 4 12 2),Mem (StoreIdx 5 12 3),AL (AddI 12 12 4),Branch (B 236),AL (SubI 12 12 4),Mem (LoadIdx 13 12 (-1)),Mem (LoadIdx 14 12 (-2)),AL (SubI 12 12 2),AL (Move 1 15),Mem (StoreIdx 1 14 4),Mem (LoadIdx 2 14 3),Mem (LoadIdx 3 14 5),Mem (LoadIdx 4 14 0),Mem (StoreIdx 14 12 0),Mem (StoreIdx 13 12 1),AL (AddI 12 12 2),AL (Move 14 12),AL (MoveI 13 150),Mem (StoreIdx 2 12 0),Mem (StoreIdx 3 12 1),Mem (StoreIdx 4 12 2),AL (AddI 12 12 3),Branch (B 186),AL (SubI 12 12 3),Mem (LoadIdx 13 12 (-1)),Mem (LoadIdx 14 12 (-2)),AL (SubI 12 12 2),AL (Move 1 15),Mem (StoreIdx 1 14 5),Mem (LoadIdx 1 14 1),Mem (LoadIdx 3 14 5),AL (Lt 1 1 3),Mem (LoadIdx 2 14 5),Mem (LoadIdx 3 14 1),AL (Eq 2 2 3),AL (Or 1 1 2),Branch (BF 1 177),Mem (LoadIdx 2 14 5),Mem (StoreIdx 14 12 0),Mem (StoreIdx 13 12 1),AL (AddI 12 12 2),AL (Move 14 12),AL (MoveI 13 173),Mem (StoreIdx 2 12 0),AL (AddI 12 12 1),Branch (B 321),AL (SubI 12 12 1),Mem (LoadIdx 13 12 (-1)),Mem (LoadIdx 14 12 (-2)),AL (SubI 12 12 2),Mem (LoadIdx 0 14 2),Mem (LoadIdx 2 14 4),Mem (LoadBaseIdx 0 0 2),AL (MoveI 1 0),AL (Eq 0 0 1),AL (Not 0 0),Branch (BT 0 110),AL (SubI 12 12 3),Branch (Ret ()),Mem (LoadIdx 0 14 0),AL (MoveI 1 62),AL (Eq 0 0 1),Branch (BF 0 194),Mem (LoadIdx 1 14 1),AL (MoveI 2 1),AL (Add 1 1 2),Mem (StoreIdx 1 14 1),Mem (LoadIdx 0 14 0),AL (MoveI 1 60),AL (Eq 0 0 1),Branch (BF 0 202),Mem (LoadIdx 1 14 1),AL (MoveI 2 1),AL (Sub 1 1 2),Mem (StoreIdx 1 14 1),Mem (LoadIdx 0 14 0),AL (MoveI 1 43),AL (Eq 0 0 1),Branch (BF 0 214),Mem (LoadIdx 1 14 2),Mem (LoadIdx 2 14 1),Mem (LoadIdx 3 14 2),Mem (LoadIdx 5 14 1),Mem (LoadBaseIdx 3 3 5),AL (MoveI 4 1),AL (Add 3 3 4),Mem (StoreBaseIdx 3 1 2),Mem (LoadIdx 0 14 0),AL (MoveI 1 45),AL (Eq 0 0 1),Branch (BF 0 226),Mem (LoadIdx 1 14 2),Mem (LoadIdx 2 14 1),Mem (LoadIdx 3 14 2),Mem (LoadIdx 5 14 1),Mem (LoadBaseIdx 3 3 5),AL (MoveI 4 1),AL (Sub 3 3 4),Mem (StoreBaseIdx 3 1 2),Mem (LoadIdx 0 14 0),AL (MoveI 1 46),AL (Eq 0 0 1),Branch (BF 0 234),Mem (LoadIdx 1 14 2),Mem (LoadIdx 2 14 1),Mem (LoadBaseIdx 1 1 2),Out (PrintC 1),Mem (LoadIdx 15 14 1),Branch (Ret ()),Mem (LoadIdx 0 14 2),Mem (LoadIdx 3 14 1),Mem (LoadBaseIdx 0 0 3),AL (MoveI 2 91),AL (Eq 0 0 2),Mem (LoadIdx 1 14 3),AL (MoveI 2 0),AL (Eq 1 1 2),AL (And 0 0 1),Branch (BF 0 265),Mem (LoadIdx 1 14 2),Mem (LoadIdx 3 14 1),Mem (LoadBaseIdx 1 1 3),AL (MoveI 2 93),AL (Eq 1 1 2),AL (Not 1 1),Branch (BF 1 264),Mem (LoadIdx 2 14 1),AL (MoveI 3 1),AL (Add 2 2 3),Mem (StoreIdx 2 14 1),Mem (LoadIdx 1 14 2),Mem (LoadIdx 3 14 1),Mem (LoadBaseIdx 1 1 3),AL (MoveI 2 93),AL (Eq 1 1 2),AL (Not 1 1),Branch (BT 1 253),Branch (B 294),Mem (LoadIdx 1 14 2),Mem (LoadIdx 4 14 1),Mem (LoadBaseIdx 1 1 4),AL (MoveI 3 93),AL (Eq 1 1 3),Mem (LoadIdx 2 14 3),AL (MoveI 3 0),AL (Eq 2 2 3),AL (Not 2 2),AL (And 1 1 2),Branch (BF 1 294),Mem (LoadIdx 2 14 2),Mem (LoadIdx 4 14 1),Mem (LoadBaseIdx 2 2 4),AL (MoveI 3 91),AL (Eq 2 2 3),AL (Not 2 2),Branch (BF 2 294),Mem (LoadIdx 3 14 1),AL (MoveI 4 1),AL (Sub 3 3 4),Mem (StoreIdx 3 14 1),Mem (LoadIdx 2 14 2),Mem (LoadIdx 4 14 1),Mem (LoadBaseIdx 2 2 4),AL (MoveI 3 91),AL (Eq 2 2 3),AL (Not 2 2),Branch (BT 2 283),Mem (LoadIdx 15 14 1),AL (MoveI 0 1),AL (Add 15 15 0),Branch (Ret ()),AL (AddI 12 12 1),AL (MoveI 0 0),Mem (StoreIdx 0 14 2),Mem (LoadIdx 0 14 2),Mem (LoadIdx 1 14 1),AL (Lt 0 0 1),Branch (BF 0 319),Mem (LoadIdx 1 14 0),Mem (LoadIdx 2 14 2),Mem (LoadBaseIdx 1 1 2),Out (Print 1),AL (MoveI 1 46),Out (PrintC 1),Mem (LoadIdx 1 14 2),AL (MoveI 2 1),AL (Add 1 1 2),Mem (StoreIdx 1 14 2),Mem (LoadIdx 0 14 2),Mem (LoadIdx 1 14 1),AL (Lt 0 0 1),Branch (BT 0 305),AL (SubI 12 12 1),Branch (Ret ()),AL (AddI 12 12 16),AL (Move 0 12),AL (SubI 0 0 14),Mem (StoreIdx 0 14 2),Mem (LoadIdx 0 14 2),AL (MoveI 1 68),Mem (StoreIdx 1 0 0),AL (MoveI 1 80),Mem (StoreIdx 1 0 1),AL (MoveI 1 32),Mem (StoreIdx 1 0 2),AL (MoveI 1 79),Mem (StoreIdx 1 0 3),AL (MoveI 1 86),Mem (StoreIdx 1 0 4),AL (MoveI 1 69),Mem (StoreIdx 1 0 5),AL (MoveI 1 82),Mem (StoreIdx 1 0 6),AL (MoveI 1 70),Mem (StoreIdx 1 0 7),AL (MoveI 1 76),Mem (StoreIdx 1 0 8),AL (MoveI 1 79),Mem (StoreIdx 1 0 9),AL (MoveI 1 87),Mem (StoreIdx 1 0 10),AL (MoveI 1 58),Mem (StoreIdx 1 0 11),AL (MoveI 1 32),Mem (StoreIdx 1 0 12),AL (MoveI 1 0),Mem (StoreIdx 1 0 13),AL (MoveI 0 0),Mem (StoreIdx 0 14 1),Mem (LoadIdx 0 14 2),Mem (LoadIdx 2 14 1),Mem (LoadBaseIdx 0 0 2),AL (MoveI 1 0),AL (Eq 0 0 1),AL (Not 0 0),Branch (BF 0 378),Mem (LoadIdx 1 14 2),Mem (LoadIdx 2 14 1),Mem (LoadBaseIdx 1 1 2),Out (PrintC 1),Mem (LoadIdx 1 14 1),AL (MoveI 2 1),AL (Add 1 1 2),Mem (StoreIdx 1 14 1),Mem (LoadIdx 0 14 2),Mem (LoadIdx 2 14 1),Mem (LoadBaseIdx 0 0 2),AL (MoveI 1 0),AL (Eq 0 0 1),AL (Not 0 0),Branch (BT 0 363),Mem (LoadIdx 0 14 0),Out (Print 0),Out PrintLn,AL (SubI 12 12 16),Branch (Ret ())]
