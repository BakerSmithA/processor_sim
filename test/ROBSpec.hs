module ROBSpec where

import Test.Hspec
import ROB as ROB
import WriteBack
import RRT (RegMap(..))
import Types

writeReg :: PhyReg -> Val -> WriteBack
writeReg p v = WriteReg p v (None False)

writeLoad :: PhyReg -> Val -> LoadData -> WriteBack
writeLoad p v ld = WriteReg p v ld

robSpec :: Spec
robSpec = describe "Reorder Buffer" $ do
    context "committable" $ do
        it "returns all instructions that can be committed in order oldest to newest" $ do
            let rob1 = ROB.empty 5

                (i1, rob2) = ROB.alloc rob1
                (i2, rob3) = ROB.alloc rob2
                (_,  rob4) = ROB.alloc rob3
                (i4, rob5) = ROB.alloc rob4

                rob6 = set i1 (writeReg 0 10) Nothing  0 rob5
                rob7 = set i2 (WriteMem 5 15) (Just 1) 1 rob6
                rob8 = set i4 (writeReg 1 5)  Nothing  2 rob7

                (cs, savedPC, _) = ROB.commitable rob8

            cs `shouldBe` [(writeReg 0 10, Nothing, NoMap), (WriteMem 5 15, Just 1, NoMap)]
            savedPC `shouldBe` Nothing

        it "returns new state of ROB" $ do
            let rob1 = ROB.empty 5

                (i1, rob2) = ROB.alloc rob1
                (i2, rob3) = ROB.alloc rob2
                (i3, rob4) = ROB.alloc rob3
                (i4, rob5) = ROB.alloc rob4

                rob6 = set i1 (writeReg 0 10) Nothing 0 rob5
                rob7 = set i2 (WriteMem 5 15) Nothing 1 rob6
                -- i3 filled in later.
                rob8 = set i4 (writeReg 1 5)  Nothing 3 rob7

                (_, _, rob9)     = ROB.commitable rob8
                rob10            = set i3 (writeReg 0 1) Nothing 2 rob9
                (cs, savedPC, _) = ROB.commitable rob10

            cs `shouldBe` [(writeReg 0 1, Nothing, NoMap), (writeReg 1 5,  Nothing, NoMap)]
            savedPC `shouldBe` Nothing

        it "allows allocation after commiting" $ do
            let rob1 = ROB.empty 5

                (i1, rob2) = ROB.alloc rob1
                (i2, rob3) = ROB.alloc rob2

                rob4         = ROB.set i1 (writeReg 0 1) Nothing 0 rob3
                (_, _, rob5) = ROB.commitable rob4

                (i3, rob6) = ROB.alloc rob5
                rob7       = ROB.set i2 (writeReg 1 2) Nothing 1 rob6
                rob8       = ROB.set i3 (writeReg 2 3) Nothing 2 rob7
                (cs, savedPC, _) = ROB.commitable rob8

            cs `shouldBe` [(writeReg 1 2, Nothing, NoMap), (writeReg 2 3, Nothing, NoMap)]
            savedPC `shouldBe` Nothing

    context "invalidate loads" $ do
        it "invalidates loads with matching addresses" $ do
            let rob1 = ROB.empty 7

                (i1, rob2) = ROB.alloc rob1
                (i2, rob3) = ROB.alloc rob2
                (_,  rob4) = ROB.alloc rob3
                (i4, rob5) = ROB.alloc rob4
                (i5, rob6) = ROB.alloc rob5
                (i6, rob7) = ROB.alloc rob6

                rob8  = set i1 (WriteMem  5 15)                Nothing 0 rob7
                rob9  = set i2 (writeLoad 0 10 (ValidLoad 1)) (Just 1) 1 rob8
                rob10 = set i4 (writeReg  1 5)                 Nothing 2 rob9
                rob11 = set i5 (writeLoad 1 5 InvalidLoad)     Nothing 3 rob10
                rob12 = set i6 (writeLoad 3 4 (ValidLoad 5))   Nothing 4 rob11

                rob13 = invalidateLoads 1 rob12
                es    = contents rob13

            es `shouldBe` [
                (Just (writeLoad 3 4 (ValidLoad 5), Nothing, 4), NoMap),
                (Just (writeLoad 1 5 InvalidLoad, Nothing, 3), NoMap),
                (Just (writeReg 1 5, Nothing, 2), NoMap),
                (Nothing, NoMap),
                (Just (writeLoad 0 10 InvalidLoad, Just 1, 1), NoMap),
                (Just (WriteMem 5 15, Nothing,0), NoMap)]

    context "flush" $ do
        it "resets all elements" $ do
            let rob1 = ROB.empty 5
                (i1, rob2) = ROB.alloc rob1
                (i2, rob3) = ROB.alloc rob2
                (_,  rob4) = ROB.alloc rob3
                (i4, rob5) = ROB.alloc rob4

                rob6 = set i1 (writeReg 0 10) Nothing 0 rob5
                rob7 = set i2 (WriteMem 5 15) Nothing 1 rob6
                -- i3 filled in later.
                rob8 = set i4 (writeReg 1 5)  Nothing 3 rob7
                rob9 = flush rob8

                es = allContents rob9

            es `shouldBe` [emptyEntry, emptyEntry, emptyEntry, emptyEntry, emptyEntry]

        it "resets queue range" $ do
            let rob1 = ROB.empty 5
                (i1, rob2) = ROB.alloc rob1
                (i2, rob3) = ROB.alloc rob2
                (_,  rob4) = ROB.alloc rob3
                (i4, rob5) = ROB.alloc rob4

                rob6 = set i1 (writeReg 0 10) Nothing 0 rob5
                rob7 = set i2 (WriteMem 5 15) Nothing 1 rob6
                -- i3 filled in later.
                rob8 = set i4 (writeReg 1 5)  Nothing 3 rob7
                rob9 = flush rob8

                es = contents rob9

            es `shouldBe` []
