module RSSpec where

import RS as RS
import Bypass as BP
import Test.Hspec

rsSpec :: Spec
rsSpec = describe "reservation station" $ do
    context "Filling in operands" $ do
        it "fills operand of op waiting for lhs" $ do
            let rs  = RS.fromList [((), WaitingL (Reg 0) 1)]
                bp  = BypassReg 0 2
                (ops, rs') = RS.fill bp rs
            ops `shouldBe` [((), BinOp 2 1)]
            rs' `shouldBe` RS.empty

        it "fills operand of op waiting for rhs" $ do
            let rs  = RS.fromList [((), WaitingR 1 (Reg 0))]
                bp  = BypassReg 0 2
                (ops, rs') = RS.fill bp rs
            ops `shouldBe` [((), BinOp 1 2)]
            rs' `shouldBe` RS.empty

        it "fills lhs of bin op waiting for both" $ do
            let rs  = RS.fromList [((), WaitingB (Reg 0) (Reg 1))]
                bp  = BypassReg 0 2
                (ops, rs') = RS.fill bp rs
            ops `shouldBe` []
            rs' `shouldBe` RS.fromList [((), WaitingR 2 (Reg 1))]

        it "fills rhs of bin op waiting for both" $ do
            let rs  = RS.fromList [((), WaitingB (Reg 0) (Reg 1))]
                bp  = BypassReg 1 2
                (ops, rs') = RS.fill bp rs
            ops `shouldBe` []
            rs' `shouldBe` RS.fromList [((), WaitingL (Reg 0) 2)]

        it "fills both lhs and rhs of op waiting for both" $ do
            let rs  = RS.fromList [((), WaitingB (Reg 0) (Reg 0))]
                bp  = BypassReg 0 2
                (ops, rs') = RS.fill bp rs
            ops `shouldBe` [((), BinOp 2 2)]
            rs' `shouldBe` RS.empty

        it "fills operand of uniary op" $ do
            let rs  = RS.fromList [((), WaitingU (Reg 0))]
                bp  = BypassReg 0 2
                (ops, rs') = RS.fill bp rs
            ops `shouldBe` [((), UniOp 2)]
            rs' `shouldBe` RS.empty

        it "fills multiple ops at once" $ do
            let rs  = RS.fromList [((), WaitingU (Mem 0)), ((), WaitingL (Mem 0) 1)]
                bp  = BypassMem 0 2
                (ops, rs') = RS.fill bp rs
            ops `shouldBe` [((), UniOp 2), ((), BinOp 2 1)]
            rs' `shouldBe` RS.empty

        it "does nothing if does not match reg" $ do
            let rs  = RS.fromList [((), WaitingU (Reg 0))]
                bp  = BypassReg 10 2
                (ops, rs') = RS.fill bp rs
            ops `shouldBe` []
            rs' `shouldBe` rs

        it "does nothign if does not match mem" $ do
            let rs  = RS.fromList [((), WaitingU (Mem 0))]
                bp  = BypassMem 10 2
                (ops, rs') = RS.fill bp rs
            ops `shouldBe` []
            rs' `shouldBe` rs
