module QueueSpec where

import Test.Hspec
import qualified Queue as Q

queueSpec :: Spec
queueSpec = describe "queue" $ do
    context "alloc" $ do
        it "allocates space for element" $ do
            let (i, q) = Q.alloc (Q.fromList [0, 0, 0, 0])
            i `shouldBe` 0
            Q.elems q `shouldBe` [0, 0, 0, 0]

        it "inserts multiple elements and wraps start" $ do
            let (i, q) = Q.alloc (snd $ Q.alloc (Q.fromList [0, 0, 0, 0]))
            i `shouldBe` 3
            Q.elems q `shouldBe` [0, 0, 0, 0]

    context "set" $ do
        it "sets element" $ do
            let (i, q) = Q.alloc (Q.fromList [0, 0, 0, 0])
                q' = Q.set i 10 q
            Q.elems q' `shouldBe` [10, 0, 0, 0]

    context "get" $ do
        it "returns element at index" $ do
            let (i, q) = Q.alloc (Q.fromList [0, 0, 0, 0])
                q' = Q.set i 5 q
                x = Q.get i q'
            x `shouldBe` 5

    context "rem" $ do
        it "removes element" $ do
            let (i, q) = Q.alloc (Q.fromList [0, 0, 0, 0])
                q'     = Q.set i 5 q
                (x, _) = Q.rem q'
            x `shouldBe` 5

        it "removes multiple elements" $ do
            let (i1, q1) = Q.alloc (Q.fromList [0, 0, 0, 0])
                q2       = Q.set i1 5 q1
                (i2, q3) = Q.alloc q2
                q4       = Q.set i2 10 q3
                (x, q5)  = Q.rem q4
                (y, _)   = Q.rem q5
            x `shouldBe` 5
            y `shouldBe` 10
