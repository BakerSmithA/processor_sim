module RSSpec where

import RS as RS
import Bypass as BP
import Test.Hspec

rsSpec :: Spec
rsSpec = describe "reservation station" $ do
    context "Filling in operands" $ do
        it "fills operand of op waiting for lhs" $ do
            let rs  = RS.fromList [('X', WaitingL (Reg 0) 1)]
                bp  = BypassReg 0 2
                ops = fst $ RS.fill bp rs
            ops `shouldBe` [BinOp 2 1]

        it "fills operand of op waiting for rhs" $ do
            pending

        it "fills lhs of op waiting for both" $ do
            pending

        it "fills rhs of op waiting for both" $ do
            pending

        it "fills both lhs and rhs of op waiting for both" $ do
            pending

        it "fills operand of uniary op" $ do
            pending

        it "fills multiple ops" $ do
            pending

        it "does nothing if does not match reg" $ do
            pending

        it "does nothign if does not match mem" $ do
            pending
