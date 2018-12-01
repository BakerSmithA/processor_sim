module RRTSpec (rrtSpec) where

import Test.Hspec
import RRT as RRT

rrtSpec :: Spec
rrtSpec = describe "register rename table" $ do
    context "fromConstRegs" $ do
        it "maps special registers to physical registers" $ do
            let maxPhy = 9
                rrt = RRT.fromConstRegs [3, 4, 5, 6, 7] maxPhy
                exp = RRT.fromMapping [] [(3,5), (4,6), (5,7), (6,8), (7,9)] [0,1,2,3,4]
            rrt `shouldBe` exp

    context "adding mapping" $ do
        it "allows adding mapping" $ do
            let rrt  = RRT.empty 3
                rrt' = RRT.ins 1 rrt
                exp  = Just (0, RRT.fromMapping [(1, 0)] [] [1, 2, 3])
            rrt' `shouldBe` exp

        it "returns const if already exists in const" $ do
            let rrt            = RRT.fromMapping [(1, 0)] [(2, 1)] [2, 3]
                Just (i, rrt') = RRT.ins 2 rrt
            rrt' `shouldBe` rrt
            i    `shouldBe` 1

        it "frees a physical register if a mapping already exists" $ do
            let rrt            = RRT.fromMapping [(1, 0)] [] [1, 2, 3]
                Just (i, rrt') = RRT.ins 1 rrt
            i `shouldBe` 1
            (frees rrt') `shouldBe` [0, 2, 3]

    context "freeing mapping" $ do
        it "allows removal of mapping" $ do
            let rrt  = RRT.fromMapping [(1, 0)] [] [1, 2, 3]
                rrt' = RRT.free 0 rrt
                exp  = RRT.empty 3
            rrt' `shouldBe` exp

        it "does nothing if no mapping exists" $ do
            let rrt  = RRT.fromMapping [(1, 0)] [] [1, 2, 3]
                rrt' = RRT.free 5 rrt
            rrt' `shouldBe` rrt

        it "does nothing if trying to free const mapping" $ do
            let rrt  = RRT.fromMapping [(1, 0)] [(2, 1)] [2, 3]
                rrt' = RRT.free 2 rrt
            rrt' `shouldBe` rrt

    context "getting mapping" $ do
        it "gets physical register for register name" $ do
            let rrt = RRT.fromMapping [(1, 0)] [] [1, 2, 3]
                phy = RRT.get 1 rrt
            phy `shouldBe` Just 0

        it "fails if no mapping exists" $ do
            let rrt = RRT.fromMapping [(1, 0)] [] [1, 2, 3]
                phy = RRT.get 5 rrt
            phy `shouldBe` Nothing

        it "first searches in consts" $ do
            let rrt = RRT.fromMapping [(1, 0)] [(1, 10)] []
                phy = RRT.get 1 rrt
            phy `shouldBe` Just 10
