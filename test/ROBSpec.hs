module ROBSpec where

import Test.Hspec
import ROB as ROB
import WriteBack

robSpec :: Spec
robSpec = describe "Reorder Buffer" $ do
    it "returns all instructions that can be committed" $ do
        let rob1 = ROB.empty 5

            (i1, rob2) = ROB.alloc rob1
            (i2, rob3) = ROB.alloc rob2
            (_,  rob4) = ROB.alloc rob3
            (i4, rob5) = ROB.alloc rob4

            rob6 = set i1 (WriteReg 0 10) Nothing rob5
            rob7 = set i2 (WriteMem 5 15) (Just 1) rob6
            rob8 = set i4 (WriteReg 1 5)  Nothing rob7

            (cs, _) = ROB.commitable rob8

        cs `shouldBe` [(WriteReg 0 10, Nothing), (WriteMem 5 15, Just 1)]

    it "returns new state of ROB" $ do
        let rob1 = ROB.empty 5

            (i1, rob2) = ROB.alloc rob1
            (i2, rob3) = ROB.alloc rob2
            (i3, rob4) = ROB.alloc rob3
            (i4, rob5) = ROB.alloc rob4

            rob6 = set i1 (WriteReg 0 10) Nothing rob5
            rob7 = set i2 (WriteMem 5 15) Nothing rob6
            rob8 = set i4 (WriteReg 1 5)  Nothing rob7

            (_, rob9) = ROB.commitable rob8
            rob10     = set i3 (WriteReg 0 1) Nothing rob9
            (cs, _)   = ROB.commitable rob10

        cs `shouldBe` [(WriteReg 0 1, Nothing), (WriteReg 1 5,  Nothing)]

    it "allows allocation after commiting" $ do
        let rob1 = ROB.empty 5

            (i1, rob2) = ROB.alloc rob1
            (i2, rob3) = ROB.alloc rob2

            rob4      = ROB.set i1 (WriteReg 0 1) Nothing rob3
            (_, rob5) = ROB.commitable rob4

            (i3, rob6) = ROB.alloc rob5
            rob7       = ROB.set i2 (WriteReg 1 2) Nothing rob6
            rob8       = ROB.set i3 (WriteReg 2 3) Nothing rob7
            (cs, _)    = ROB.commitable rob8

        cs `shouldBe` [(WriteReg 1 2, Nothing), (WriteReg 2 3, Nothing)]
