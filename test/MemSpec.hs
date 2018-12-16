module MemSpec where

import Test.Hspec
import qualified Mem as Mem

memSpec :: Spec
memSpec = describe "mem" $ do
    context "take" $ do
        it "takes elements" $ do
            let mem = Mem.fromList ['a', 'b', 'c', 'd']
                xs  = Mem.take 2 1 mem
            xs `shouldBe` ['b', 'c']

        it "stops at the last element" $ do
            let mem = Mem.fromList ['a', 'b', 'c', 'd']
                xs  = Mem.take 10 1 mem
            xs `shouldBe` ['b', 'c', 'd']
