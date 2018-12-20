module RSSpec where

import Test.Hspec
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Control.Monad.Identity
import Types
import Instr
import qualified RS as RS

-- Used to construct a test method for supplying register values.
regVal :: [(PhyReg, Val)] -> ROBIdx -> PhyReg -> Identity (Maybe Val)
regVal vs _ p = return $ Map.lookup p (Map.fromList vs)

memVal :: [(Addr, Val)] -> ROBIdx -> Addr -> Identity Val
memVal vs _ a = return $ fromJust $ Map.lookup a (Map.fromList vs)

rsSpec :: Spec
rsSpec = describe "reservation station" $ do
    context "load store queue" $ do
        it "runs store instructions" $ do
            let rv  = regVal [(0, 5), (1, 10), (3, 20)]
                mv  = memVal []
                ins = [(StoreIdx     (Left 0) (Right 5) 10,       2, Nothing, 0)
                     , (StoreBaseIdx (Left 0) (Right 3) (Left 5), 0, Nothing, 1)
                     , (StoreBaseIdx (Left 0) (Left 1)  (Left 3), 1, Just 3,  2)]
                rs1 = RS.fromList ins
                rs2 = runIdentity (RS.fillMemRS rv mv rs1)
                (exec1, rs3) = RS.promoteMemRS rs2
                (exec2, rs4) = RS.promoteMemRS rs3
                (exec3, _)   = RS.promoteMemRS rs4

            exec1 `shouldBe` Just (EStore 5 30, 1, Just 3, 2)
            exec2 `shouldBe` Just (EStore 5 15, 2, Nothing, 0)
            exec3 `shouldBe` Nothing

        it "runs load instructions" $ do
            let rv  = regVal [(0, 5), (1, 10), (3, 90)]
                mv  = memVal [(100, 1), (200, 2), (300, 3)]
                ins = [(LoadIdx     (5, Nothing) (Right 200) 0,        0, Nothing, 0)
                     , (LoadBaseIdx (6, Nothing) (Left 1)    (Left 3), 1, Nothing, 1)
                     , (LoadIdx     (0, Nothing) (Left 10)   5,        2, Nothing, 2)]
                rs1 = RS.fromList ins
                rs2 = runIdentity (RS.fillMemRS rv mv rs1)
                (exec1, rs3) = RS.promoteMemRS rs2
                (exec2, rs4) = RS.promoteMemRS rs3
                (exec3, _)   = RS.promoteMemRS rs4

            exec1 `shouldBe` Just (ELoad 6 1 100, 1, Nothing, 1)
            exec2 `shouldBe` Just (ELoad 5 2 200, 0, Nothing, 0)
            exec3 `shouldBe` Nothing

    context "ArithLogicRS" $ do
        it "runs" $ do
            let rv  = regVal [(0, 5), (1, 10), (2, 20)]
                ins = [(AddI 0 (Left 1)  6,        0, Just 3, 0)
                     , (Div  1 (Right 6) (Left 3), 1, Nothing, 1)]
                rs1 = RS.fromList ins
                rs2 = runIdentity (RS.fillALRS rv rs1)
                (exec1, rs3) = RS.promoteALRS rs2
                (exec2, _)   = RS.promoteALRS rs3

            exec1 `shouldBe` Just (AddI 0 10 6, 0, Just 3, 0)
            exec2 `shouldBe` Nothing

    context "BranchRS" $ do
        it "runs" $ do
            let rv  = regVal [(0, 5), (14, 5)]
                ins = [(BT  (Left 0) 10, 0, Nothing, 0)
                     , (Ret (Left 14),   1, Nothing, 1)]
                rs1  = RS.fromList ins
                rs2 = runIdentity (RS.fillBRS rv rs1)
                (exec1, rs3) = RS.promoteBRS rs2
                (exec2, rs4) = RS.promoteBRS rs3
                (exec3, _)   = RS.promoteBRS rs4

            exec1 `shouldBe` Just (Ret 5, 1, Nothing, 1)
            exec2 `shouldBe` Just (BT 5 10, 0, Nothing, 0)
            exec3 `shouldBe` Nothing

    context "OutRS" $ do
        it "runs" $ do
            let rv  = regVal [(0, 5)]
                ins = [(Print (Left 0), 0, Nothing, 0)
                     , (PrintLn,        1, Nothing, 1)]
                rs1  = RS.fromList ins
                rs2 = runIdentity (RS.fillOutRS rv rs1)
                (exec1, rs3) = RS.promoteOutRS rs2
                (exec2, rs4) = RS.promoteOutRS rs3
                (exec3, _)   = RS.promoteOutRS rs4

            exec1 `shouldBe` Just (PrintLn, 1, Nothing, 1)
            exec2 `shouldBe` Just (Print 5, 0, Nothing, 0)
            exec3 `shouldBe` Nothing
