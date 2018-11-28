module RRTSpec (rrtSpec) where

import Test.Hspec
import RRT as RRT

rrtSpec :: Spec
rrtSpec = describe "register rename table" $ do
    it "allows adding mapping" $ do
        let rrt  = RRT.empty 3
            rrt' = RRT.ins 1 rrt
            exp  = RRT.fromMapping [(1, 0)] [1, 2, 3]
        rrt' `shouldBe` Just exp
