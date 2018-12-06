module RSSpec where

import Test.Hspec
import Instr
import State as St
import RS as RS

rsSpec :: Spec
rsSpec = describe "reservation station" $ do
    context "add" $ do
        it "adds a decoded instr and fills in no operands" $ do
            let di  = LoadIdx 0 2 10 :: DInstr
                rs  = RS.add di RS.empty
                exp = RS.fromList [LoadIdx 0 (Left 2) 10]
            rs `shouldBe` exp
