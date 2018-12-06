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
            let di  = (LoadIdx 0 2 10, 0, Nothing) :: DInstrIdx
                rs  = RS.add di RS.empty
                exp = RS.fromList [(LoadIdx 0 (Left 2) 10, 0, Nothing)]
            rs `shouldBe` exp

    context "run" $ do
        it "fills in operands and promotes instructions that are filled" $ do
            let regFile  = regVal [(0, 2), (1, 5), (4, 10)]
                rsInstrs = [(Move 0 (Left 1), 0, Just 1),
                            (StoreBaseIdx (Left 0) (Left 3) (Left 5), 1, Nothing),
                            (AddI 3 (Left 5) 10, 2, Nothing)]
                out      = RS.run regFile (const True) (RS.fromList rsInstrs)

                expExec  = [(Move 0 5, 0, Just 1)]
                expInstr = [(StoreBaseIdx (Right 2) (Left 3) (Left 5), 1, Nothing), (AddI 3 (Left 5) 10, 2, Nothing)]
                expRs    = RS.fromList expInstr
                expOut   = return (expExec, expRs)

            out `shouldBe` expOut

    context "tryFill" $ do
        it "fills in operands for which values can be retrieved" $ do
            let regFile  = regVal [(0, 2), (1, 5), (4, 10)]
                rsInstrs = [(Move 0 (Left 1), 0, Nothing),
                            (StoreBaseIdx (Left 0) (Left 3) (Left 5), 1, Nothing),
                            (AddI 3 (Left 5) 10, 2, Nothing)]
                rs       = RS.tryFill regFile (RS.fromList rsInstrs)

                expInstr = [(Move 0 (Right 5), 0, Nothing),
                            (StoreBaseIdx (Right 2) (Left 3) (Left 5), 1, Nothing),
                            (AddI 3 (Left 5) 10, 2, Nothing)]
                exp      = return (RS.fromList expInstr)

            rs `shouldBe` exp

    context "promote" $ do
        it "removes instructions that have all operands filled" $ do
            let rs         = RS.fromList [(Move 0 (Right 5), 0, Nothing),
                                          (Mult 1 (Right 3) (Left 0), 1, Nothing),
                                          (Print (Right 3), 2, Nothing)]
                (eis, rs') = RS.promote (const True) rs

                expExec    = [(Move 0 5, 0, Nothing), (Print 3, 2, Nothing)] :: [EInstrIdx]
                expRs      = RS.fromList [(Mult 1 (Right 3) (Left 0), 1, Nothing)]

            eis `shouldBe` expExec
            rs' `shouldBe` expRs

        it "does not remove operands if the condition fails" $ do
            let rs         = RS.fromList [(Move 0 (Right 5), 0, Nothing),
                                          (Mult 1 (Right 3) (Left 0), 1, Nothing),
                                          (Print (Right 3), 2, Nothing)]
                (eis, rs') = RS.promote (const False) rs

            eis `shouldBe` []
            rs' `shouldBe` rs
