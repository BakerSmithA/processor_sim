module RRTSpec (rrtSpec) where

import Test.Hspec
import RRT as RRT

rrtSpec :: Spec
rrtSpec = describe "register rename table" $ do
    context "fromRegs" $ do
        it "maps to physical registers" $ do
            let maxPhy = 9
                rrt = RRT.fromRegs [3, 4, 5, 6, 7] maxPhy
                exp = RRT.fromMapping [(3,4), (4,3), (5,2), (6,1), (7,0)] [5, 6, 7, 8, 9] 5
            rrt `shouldBe` exp

    context "adding mapping" $ do
        it "allows adding mapping" $ do
            let rrt  = RRT.empty 3 0
                rrt' = RRT.ins 1 rrt
                exp  = Just (0, RRT.fromMapping [(1, 0)] [1, 2, 3] 0, Nothing)
            rrt' `shouldBe` exp

        it "frees a physical register if a mapping already exists" $ do
            let rrt                   = RRT.fromMapping [(1, 0)] [1, 2, 3] 0
                Just (i, rrt', freed) = RRT.ins 1 rrt
            i `shouldBe` 1
            (frees rrt') `shouldBe` [2, 3, 0]
            freed `shouldBe` Just 0

    context "getting mapping" $ do
        it "gets physical register for register name" $ do
            let rrt = RRT.fromMapping [(1, 0)] [1, 2, 3] 0
                phy = RRT.get 1 rrt
            phy `shouldBe` Just 0

        it "fails if no mapping exists" $ do
            let rrt = RRT.fromMapping [(1, 0)] [1, 2, 3] 0
                phy = RRT.get 5 rrt
            phy `shouldBe` Nothing
