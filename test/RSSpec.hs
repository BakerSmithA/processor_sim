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
                ins = [(StoreIdx     (Left 0) (Right 5) 10,       2, Nothing)
                     , (StoreBaseIdx (Left 0) (Right 3) (Left 5), 0, Nothing)
                     , (StoreBaseIdx (Left 0) (Left 1)  (Left 3), 1, Just 3)]
                rs  = RS.fromList ins
                (execs, rs') = runIdentity (RS.runMemRS rv mv rs)

            execs `shouldBe` [(EStore 5 15, 2, Nothing), (EStore 5 30, 1, Just 3)] -- 30 from 10+20 stored in registers.
            rs'   `shouldBe` RS.fromList [(StoreBaseIdx (Right 5) (Right 3) (Left 5), 0, Nothing)]

        it "runs load instructions" $ do
            let rv  = regVal [(0, 5), (1, 10), (3, 90)]
                mv  = memVal [(100, 1), (200, 2), (300, 3)]
                ins = [(LoadIdx     (5, Nothing) (Right 200) 0,        0, Nothing)
                     , (LoadBaseIdx (6, Nothing) (Left 1)    (Left 3), 1, Nothing)
                     , (LoadIdx     (0, Nothing) (Left 10)   5,        2, Nothing)]
                rs = RS.fromList ins
                (execs, rs') = runIdentity (RS.runMemRS rv mv rs)

            rs' `shouldBe` RS.fromList [(LoadIdx (0, Nothing) (Left 10) 5, 2, Nothing)]
            execs `shouldBe` [(ELoad 5 2 200, 0, Nothing)
                            , (ELoad 6 1 100, 1, Nothing)]

    context "ArithLogicRS" $ do
        it "runs" $ do
            let rv  = regVal [(0, 5), (1, 10), (2, 20)]
                ins = [(AddI 0 (Left 1)  6,        0, Just 3)
                     , (Div  1 (Right 6) (Left 3), 1, Nothing)]
                rs = RS.fromList ins
                (execs, rs') = runIdentity (RS.runAL rv rs)

            rs'   `shouldBe` RS.fromList [(Div 1 (Right 6) (Left 3), 1, Nothing)]
            execs `shouldBe` [(AddI 0 10 6, 0, Just 3)]

    context "BranchRS" $ do
        it "runs" $ do
            let rv  = regVal [(0, 5), (14, 5)]
                ins = [(BT  (Left 0) 10, 0, Nothing)
                     , (Ret (Left 14),   1, Nothing)]
                rs  = RS.fromList ins
                (execs, rs') = runIdentity (RS.runB rv rs)

            rs'   `shouldBe` []
            execs `shouldBe` [(BT 5 10, 0, Nothing), (Ret 5, 1, Nothing)]

    context "OutRS" $ do
        it "runs" $ do
            let rv  = regVal [(0, 5)]
                ins = [(Print (Left 0), 0, Nothing)
                     , (PrintLn,        1, Nothing)]
                rs  = RS.fromList ins
                (execs, rs') = runIdentity (RS.runOut rv rs)

            rs'   `shouldBe` []
            execs `shouldBe` [(Print 5, 0, Nothing), (PrintLn, 1, Nothing)]
