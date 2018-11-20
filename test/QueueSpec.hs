module QueueSpec where

import Test.Hspec
import qualified Queue as Q

queueSpec :: Spec
queueSpec = describe "queue" $ do
    context "ins" $ do
        it "inserts element" $ do
            let q = Q.ins 1 (Q.fromList [0, 0, 0, 0])
            Q.elems q `shouldBe` [1, 0, 0, 0]

        it "inserts multiple elements and wraps start" $ do
            let q = Q.ins 2 (Q.ins 1 (Q.fromList [0, 0, 0, 0]))
            Q.elems q `shouldBe` [1, 0, 0, 2]
