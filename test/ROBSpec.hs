module ROBSpec where

import Test.Hspec
import ROB as ROB
import WriteBack
import Types

writeReg :: PhyReg -> Val -> WriteBack
writeReg p v = WriteReg p v Nothing

writeLoad :: PhyReg -> Val -> Addr -> Valid -> WriteBack
writeLoad p v a valid = WriteReg p v (Just (LoadData a valid))

robSpec :: Spec
robSpec = describe "Reorder Buffer" $ do
    context "committable" $ do
        it "returns all instructions that can be committed" $ do
            let rob1 = ROB.empty 5

                (i1, rob2) = ROB.alloc rob1
                (i2, rob3) = ROB.alloc rob2
                (_,  rob4) = ROB.alloc rob3
                (i4, rob5) = ROB.alloc rob4

                rob6 = set i1 (writeReg 0 10) Nothing  0 rob5
                rob7 = set i2 (WriteMem 5 15) (Just 1) 1 rob6
                rob8 = set i4 (writeReg 1 5)  Nothing  2 rob7

                (cs, _) = ROB.commitable rob8

            cs `shouldBe` [(writeReg 0 10, Nothing, 0), (WriteMem 5 15, Just 1, 1)]

        it "returns new state of ROB" $ do
            let rob1 = ROB.empty 5

                (i1, rob2) = ROB.alloc rob1
                (i2, rob3) = ROB.alloc rob2
                (i3, rob4) = ROB.alloc rob3
                (i4, rob5) = ROB.alloc rob4

                rob6 = set i1 (writeReg 0 10) Nothing 0 rob5
                rob7 = set i2 (WriteMem 5 15) Nothing 1 rob6
                rob8 = set i4 (writeReg 1 5)  Nothing 2 rob7

                (_, rob9) = ROB.commitable rob8
                rob10     = set i3 (writeReg 0 1) Nothing 3 rob9
                (cs, _)   = ROB.commitable rob10

            cs `shouldBe` [(writeReg 0 1, Nothing, 3), (writeReg 1 5,  Nothing, 2)]

        it "allows allocation after commiting" $ do
            let rob1 = ROB.empty 5

                (i1, rob2) = ROB.alloc rob1
                (i2, rob3) = ROB.alloc rob2

                rob4      = ROB.set i1 (writeReg 0 1) Nothing 0 rob3
                (_, rob5) = ROB.commitable rob4

                (i3, rob6) = ROB.alloc rob5
                rob7       = ROB.set i2 (writeReg 1 2) Nothing 1 rob6
                rob8       = ROB.set i3 (writeReg 2 3) Nothing 2 rob7
                (cs, _)    = ROB.commitable rob8

            cs `shouldBe` [(writeReg 1 2, Nothing, 1), (writeReg 2 3, Nothing, 2)]

    context "invalidate loads" $ do
        it "invalidates loads with matching addresses" $ do
            let rob1 = ROB.empty 6

                (i1, rob2) = ROB.alloc rob1
                (i2, rob3) = ROB.alloc rob2
                (_,  rob4) = ROB.alloc rob3
                (i4, rob5) = ROB.alloc rob4
                (i5, rob6) = ROB.alloc rob5

                rob7  = set i1 (WriteMem  5 15)        Nothing  0 rob6
                rob8  = set i2 (writeLoad 0 10 1 True) (Just 1) 1 rob7
                rob9  = set i4 (writeReg  1 5)         Nothing  2 rob8
                rob10 = set i5 (writeLoad 1 5 4 False) Nothing  3 rob9

                rob11 = invalidateLoads 1 rob10
                es    = contents rob11

            es `shouldBe` [
                Just (writeLoad 1 5 4 False, Nothing, 3),
                Just (writeReg 1 5, Nothing, 2),
                Nothing,
                Just (writeLoad 0 10 1 False, Just 1, 1),
                Just (WriteMem 5 15,Nothing,0)]
