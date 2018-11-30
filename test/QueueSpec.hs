module QueueSpec where

import Test.Hspec
import qualified Queue as Q

queueSpec :: Spec
queueSpec = describe "queue" $ do
    context "enq" $ do
        it "enqueues element" $ do
            let (_, q) = Q.enq 5 (Q.fromList [0, 0, 0, 0])
            Q.elems q `shouldBe` [5]

        it "inserts multiple elements" $ do
            let q = snd $ Q.enq 15 (snd $ Q.enq 10 (snd $ Q.enq 5 (Q.fromList [0, 0, 0, 0])))
            Q.elems q `shouldBe` [15, 10, 5]

    context "set" $ do
        it "sets element" $ do
            let (i, q) = Q.enq 10 (Q.fromList [0, 0, 0, 0])
                q' = Q.set i 5 q
            Q.elems q' `shouldBe` [5]

    context "get" $ do
        it "returns element at index" $ do
            let (i, q) = Q.enq 5 (Q.fromList [0, 0, 0, 0])
                x = Q.get i q
            x `shouldBe` 5

    context "peek" $ do
        it "returns element at head" $ do
            let (_, q) = Q.enq 5 (Q.fromList [0, 0, 0, 0])
                x      = Q.peek q
            x `shouldBe` 5

        it "returns head after removing elements" $ do
            let (_, q1) = Q.enq 5 (Q.fromList [0, 0, 0, 0])
                (_, q2) = Q.enq 10 q1
                x       = Q.peek q2
                q3      = Q.rem q2
                y       = Q.peek q3
            x `shouldBe` 5
            y `shouldBe` 10

    context "find newest" $ do
        it "returns element closest to tail (most recent) that matches predicate" $ do
            let q = snd $ Q.enq 15 (snd $ Q.enq 10 (snd $ Q.enq 5 (Q.fromList [0, 0, 0, 0])))
                x = Q.findNewest (>9) q
            x `shouldBe` Just 15

        it "returns Nothing if no matches" $ do
            let q = snd $ Q.enq 15 (snd $ Q.enq 10 (snd $ Q.enq 5 (Q.fromList [0, 0, 0, 0])))
                x = Q.findNewest (>20) q
            x `shouldBe` Nothing

        it "returns Nothing if no matches" $ do
            let q = snd $ Q.enq 15 (snd $ Q.enq 10 (snd $ Q.enq 5 (Q.fromList [0, 0, 0, 0])))
                x = Q.findNewest (<3) q
            x `shouldBe` Nothing
