module RSSpec where

import Test.Hspec
import qualified Data.Map as Map
import Types
import Instr
import RS as RS
import Control.Monad.Identity

-- Used to construct a test method for supplying register values.
regVal :: [(PhyReg, Val)] -> PhyReg -> Identity (Maybe Val)
regVal vs p = return $ Map.lookup p (Map.fromList vs)

rsSpec :: Spec
rsSpec = describe "reservation station" $ do
    context "add" $ do
        it "adds a decoded instr and fills in no operands" $ do
            let di  = (LoadIdx 0 2 10, 0) :: DInstrIdx
                rs  = RS.add di RS.empty
                exp = RS.fromList [(LoadIdx 0 (Left 2) 10, 0)]
            rs `shouldBe` exp

    context "tryFill" $ do
        it "fills in operands for which values can be retrieved" $ do
            let regFile  = regVal [(0, 2), (1, 5), (4, 10)]
                rsInstrs = [(Move 0 (Left 1), 0),
                            (StoreBaseIdx (Left 0) (Left 3) (Left 5), 1),
                            (AddI 3 (Left 5) 10, 2)]
                rs       = RS.tryFill regFile (RS.fromList rsInstrs)

                expInstr = [(Move 0 (Right 5), 0),
                            (StoreBaseIdx (Right 2) (Left 3) (Left 5), 1),
                            (AddI 3 (Left 5) 10, 2)]
                exp      = return (RS.fromList expInstr)

            rs `shouldBe` exp

    context "promote" $ do
        it "removes instructions that have all operands filled" $ do
            let rs         = RS.fromList [(Move 0 (Right 5), 0),
                                          (Mult 1 (Right 3) (Left 0), 1),
                                          (Print (Right 3), 2)]
                (eis, rs') = RS.promote (const True) rs

                expExec    = [(Move 0 5, 0), (Print 3, 2)] :: [EInstrIdx]
                expRs      = RS.fromList [(Mult 1 (Right 3) (Left 0), 1)]

            eis `shouldBe` expExec
            rs' `shouldBe` expRs

        it "does not remove operands if the condition fails" $ do
            let rs         = RS.fromList [(Move 0 (Right 5), 0),
                                          (Mult 1 (Right 3) (Left 0), 1),
                                          (Print (Right 3), 2)]
                (eis, rs') = RS.promote (const False) rs
                
            eis `shouldBe` []
            rs' `shouldBe` rs
