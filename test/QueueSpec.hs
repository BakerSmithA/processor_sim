module QueueSpec where

import Test.Hspec
import qualified Queue as Q

queueSpec :: Spec
queueSpec = describe "queue" $ do
    context "enq" $ do
        it "enqueues element" $ do
            let (_, q) = Q.enq 5 (Q.fromList [0, 0, 0, 0])
            Q.elemsNewOld q `shouldBe` [5]

        it "inserts multiple elements" $ do
            let q = snd $ Q.enq 15 (snd $ Q.enq 10 (snd $ Q.enq 5 (Q.fromList [0, 0, 0, 0])))
            Q.elemsNewOld q `shouldBe` [15, 10, 5]

    context "set" $ do
        it "sets element" $ do
            let (i, q) = Q.enq 10 (Q.fromList [0, 0, 0, 0])
                q' = Q.set i 5 q
            Q.elemsNewOld q' `shouldBe` [5]

    context "get" $ do
        it "returns element at index" $ do
            let (i, q) = Q.enq 5 (Q.fromList [0, 0, 0, 0])
                x = Q.get i q
            x `shouldBe` 5

    context "peek" $ do
        it "returns element at head" $ do
            let (_, q) = Q.enq 5 (Q.fromList [0, 0, 0, 0])
                x      = Q.peek q
            x `shouldBe` Just 5

        it "returns head after removing elements" $ do
            let (_, q1) = Q.enq 5 (Q.fromList [0, 0, 0, 0])
                (_, q2) = Q.enq 10 q1
                x       = Q.peek q2
                q3      = Q.rem q2 0
                y       = Q.peek q3
            x `shouldBe` Just 5
            y `shouldBe` Just 10

        it "returns Nothing if queue is empty" $ do
            let q = Q.fromList [0, 0, 0, 0]
            Q.peek q `shouldBe` Nothing

    context "find newest to oldest" $ do
        it "returns newest element that matches predicate 1" $ do
            let q0        = Q.fromList [0, 0, 0, 0]
                (_, q1) = Q.enq 5  q0
                (_, q2) = Q.enq 10 q1
                (_, q3) = Q.enq 15 q2
                x         = Q.find Q.NewToOld (>1) q3
            x `shouldBe` Just 15

        it "returns newest element that matches predicate 1" $ do
            let q0        = Q.fromList [0, 0, 0, 0]
                (_, q1) = Q.enq 5  q0
                (_, q2) = Q.enq 10 q1
                (_, q3) = Q.enq 15 q2
                x         = Q.find Q.NewToOld (<14) q3
            x `shouldBe` Just 10

        it "returns newest element that matches predicate 1" $ do
            let q0        = Q.fromList [0, 0, 0, 0]
                (_, q1) = Q.enq 5  q0
                (_, q2) = Q.enq 10 q1
                (_, q3) = Q.enq 15 q2
                x         = Q.find Q.NewToOld (<9) q3
            x `shouldBe` Just 5

        it "returns nothing if no element matches" $ do
            let q0       = Q.fromList [0, 0, 0, 0]
                (_, q1) = Q.enq 5  q0
                (_, q2) = Q.enq 10 q1
                (_, q3) = Q.enq 15 q2
                x        = Q.find Q.NewToOld (>100) q3
            x `shouldBe` Nothing

    context "find oldest to newest" $ do
        it "returns oldest element that matches predicate 1" $ do
            let q0        = Q.fromList [0, 0, 0, 0]
                (_, q1) = Q.enq 5  q0
                (_, q2) = Q.enq 10 q1
                (_, q3) = Q.enq 15 q2
                x         = Q.find Q.OldToNew (>1) q3
            x `shouldBe` Just 5

        it "returns oldest element that matches predicate 1" $ do
            let q0        = Q.fromList [0, 0, 0, 0]
                (_, q1) = Q.enq 5  q0
                (_, q2) = Q.enq 10 q1
                (_, q3) = Q.enq 15 q2
                x         = Q.find Q.OldToNew (>7) q3
            x `shouldBe` Just 10

        it "returns oldest element that matches predicate 1" $ do
            let q0        = Q.fromList [0, 0, 0, 0]
                (_, q1) = Q.enq 5  q0
                (_, q2) = Q.enq 10 q1
                (_, q3) = Q.enq 15 q2
                x         = Q.find Q.OldToNew (>11) q3
            x `shouldBe` Just 15

        it "returns oldest if no element matches" $ do
            let q0       = Q.fromList [0, 0, 0, 0]
                (_, q1) = Q.enq 5  q0
                (_, q2) = Q.enq 10 q1
                (_, q3) = Q.enq 15 q2
                x        = Q.find Q.OldToNew (>100) q3
            x `shouldBe` Nothing

    context "find sub newest to oldest" $ do
        it "returns newest element that matches predicate 1" $ do
            let q0        = Q.fromList [0, 0, 0, 0]
                (_,   q1) = Q.enq 5  q0
                (i10, q2) = Q.enq 10 q1
                (_,   q3) = Q.enq 15 q2
                x         = Q.find (Q.SubNewToOld i10) (>1) q3
            x `shouldBe` Just 5

        it "returns newest element that matches predicate 2" $ do
            let q0       = Q.fromList [0, 0, 0, 0]
                (i5, q1) = Q.enq 5  q0
                (_,  q2) = Q.enq 10 q1
                (_,  q3) = Q.enq 15 q2
                x        = Q.find (Q.SubNewToOld i5) (>1) q3
            x `shouldBe` Nothing

        it "returns newest element that matches predicate 3" $ do
            let q0       = Q.fromList [0, 0, 0, 0]
                (_,   q1) = Q.enq 5  q0
                (_,   q2) = Q.enq 10 q1
                (i15, q3) = Q.enq 15 q2
                x        = Q.find (Q.SubNewToOld i15) (>1) q3
            x `shouldBe` Just 10

        it "returns nothing if no element matches" $ do
            let q0       = Q.fromList [0, 0, 0, 0]
                (i5, q1) = Q.enq 5  q0
                (_,  q2) = Q.enq 10 q1
                (_,  q3) = Q.enq 15 q2
                x        = Q.find (Q.SubNewToOld i5) (>100) q3
            x `shouldBe` Nothing

    context "mapQ" $ do
        it "maps elements" $ do
            let q0 = Q.fromList [0, 0, 0, 0]
                (_, q1) = Q.enq 5  q0
                (_, q2) = Q.enq 10 q1
                (_, q3) = Q.enq 15 q2
                q4 = Q.mapQ (+10) q3
            Q.elemsNewOld q4 `shouldBe` [25, 20, 15]
