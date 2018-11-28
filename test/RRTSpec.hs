module RRTSpec (rrtSpec) where

import Test.Hspec
import RRT as RRT

rrtSpec :: Spec
rrtSpec = describe "register rename table" $ do
    context "adding mapping" $ do
        it "allows adding mapping" $ do
            let rrt  = RRT.empty 3
                rrt' = RRT.ins 1 rrt
                exp  = Just (RRT.fromMapping [(1, 0)] [1, 2, 3])
            rrt' `shouldBe` exp

    context "freeing mapping" $ do
        it "allows removal of mapping" $ do
            let rrt  = RRT.fromMapping [(1, 0)] [1, 2, 3]
                rrt' = RRT.free 1 rrt
                exp  = Just (RRT.empty 3)
            rrt' `shouldBe` exp

        it "fails if no mapping exists" $ do
            let rrt  = RRT.fromMapping [(1, 0)] [1, 2, 3]
                rrt' = RRT.free 5 rrt
            rrt' `shouldBe` Nothing

    context "getting mapping" $ do
        it "gets physical register for register name" $ do
            let rrt = RRT.fromMapping [(1, 0)] [1, 2, 3]
                phy = RRT.get 1 rrt
            phy `shouldBe` Just 0

        it "fails if no mapping exists" $ do
            let rrt = RRT.fromMapping [(1, 0)] [1, 2, 3]
                phy = RRT.get 5 rrt
            phy `shouldBe` Nothing
