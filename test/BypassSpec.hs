module BypassSpec where

import Test.Hspec
import Bypass as BP
import WriteBack

bypassSpec :: Spec
bypassSpec = describe "bypass" $ do
    context "fromWbs" $ do
        it "only puts mem or reg writes in bypass" $ do
            let wbs = [Terminate, NoOp, WriteReg 0 1 Nothing, WriteMem 10 20, WritePrint "x"]
                bp  = BP.fromWbs wbs
                exp = BP.fromList [BypassReg 0 1, BypassMem 10 20]
            bp `shouldBe` exp

    context "regVal" $ do
        it "returns value of register" $ do
            let bp  = BP.fromList [BypassReg 0 1, BypassReg 1 2, BypassMem 10 20]
                val = BP.regVal 1 bp
            val `shouldBe` Just 2

        it "returns Nothing if no matching register" $ do
            let bp  = BP.fromList [BypassReg 0 1, BypassReg 1 2, BypassMem 10 20]
                val = BP.regVal 10 bp
            val `shouldBe` Nothing

    context "memVal" $ do
        it "returns value of memory" $ do
            let bp  = BP.fromList [BypassMem 0 1, BypassReg 1 2, BypassMem 10 20]
                val = BP.memVal 10 bp
            val `shouldBe` Just 20

        it "returns Nothing if no matching address" $ do
            let bp  = BP.fromList [BypassMem 0 1, BypassReg 1 2, BypassMem 10 20]
                val = BP.memVal 11 bp
            val `shouldBe` Nothing
