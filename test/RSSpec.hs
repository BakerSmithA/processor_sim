module RSSpec where

import Test.Hspec
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Control.Monad.Identity
import Types
import Instr
import RS as RS

-- Used to construct a test method for supplying register values.
regVal :: [(PhyReg, Val)] -> PhyReg -> Identity (Maybe Val)
regVal vs p = return $ Map.lookup p (Map.fromList vs)

memVal :: [(Addr, Val)] -> Addr -> Identity Val
memVal vs a = return $ fromJust $ Map.lookup a (Map.fromList vs)

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
                (execs, rs') = runIdentity (RS.runLSQ rv mv rs)

            execs `shouldBe` [(EStore 5 15, 2, Nothing), (EStore 5 30, 1, Just 3)] -- 30 from 10+20 stored in registers.
            rs'   `shouldBe` RS.fromList [(StoreBaseIdx (Right 5) (Right 3) (Left 5), 0, Nothing)]

        it "runs load instructions" $ do
            let rv  = regVal [(0, 5), (1, 10), (3, 90)]
                mv  = memVal [(100, 1), (200, 2), (300, 3)]
                ins = [(LoadIdx     (5, Nothing) (Right 200) 0,        0, Nothing)
                     , (LoadBaseIdx (6, Nothing) (Left 1)    (Left 3), 1, Nothing)
                     , (LoadIdx     (0, Nothing) (Left 10)   5,        2, Nothing)]
                rs = RS.fromList ins
                (execs, rs') = runIdentity (RS.runLSQ rv mv rs)

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

-- data MemInstr rDst rSrc
--     = LoadIdx      rDst rSrc Val  -- r <- [[base] + offset]
--     | LoadBaseIdx  rDst rSrc rSrc -- r <- [[base] + [R_offset]]
--     | StoreIdx     rSrc rSrc Val  -- r -> [[base] + offset]
--     | StoreBaseIdx rSrc rSrc rSrc -- r -> [[base] + [R_offset]]
--     deriving (Eq, Show)

--
-- rsSpec :: Spec
-- rsSpec = describe "reservation station" $ do
--     context "add" $ do
--         it "adds a decoded instr and fills in no operands" $ do
--             let di  = (loadIdx 0 2 10, 0, Nothing) :: DPipeInstr
--                 rs  = RS.add di RS.empty
--                 exp = RS.fromList [(loadIdx 0 (Left 2) 10, 0, Nothing)]
--             rs `shouldBe` exp
--
--     context "run" $ do
--         it "fills in operands and promotes instructions that are filled" $ do
--             let regFile  = regVal [(0, 2), (1, 5), (4, 10)]
--                 rsInstrs = [(move 0 (Left 1), 0, Just 1),
--                             (storeBaseIdx (Left 0) (Left 3) (Left 5), 1, Nothing),
--                             (addI 3 (Left 5) 10, 2, Nothing)]
--                 out      = RS.run regFile (const True) (RS.fromList rsInstrs)
--
--                 expExec  = [(move 0 5, 0, Just 1)]
--                 expInstr = [(storeBaseIdx (Right 2) (Left 3) (Left 5), 1, Nothing),
--                             (addI 3 (Left 5) 10, 2, Nothing)]
--                 expRs    = RS.fromList expInstr
--                 expOut   = return (expExec, expRs)
--
--             out `shouldBe` expOut
--
--     context "tryFill" $ do
--         it "fills in operands for which values can be retrieved" $ do
--             let regFile  = regVal [(0, 2), (1, 5), (4, 10)]
--                 rsInstrs = [(move 0 (Left 1), 0, Nothing),
--                             (storeBaseIdx (Left 0) (Left 3) (Left 5), 1, Nothing),
--                             (addI 3 (Left 5) 10, 2, Nothing)]
--                 rs       = RS.tryFill regFile (RS.fromList rsInstrs)
--
--                 expInstr = [(move 0 (Right 5), 0, Nothing),
--                             (storeBaseIdx (Right 2) (Left 3) (Left 5), 1, Nothing),
--                             (addI 3 (Left 5) 10, 2, Nothing)]
--                 exp      = return (RS.fromList expInstr)
--
--             rs `shouldBe` exp
--
--     context "promote" $ do
--         it "removes instructions that have all operands filled" $ do
--             let rs         = RS.fromList [(move 0 (Right 5), 0, Nothing),
--                                           (mult 1 (Right 3) (Left 0), 1, Nothing),
--                                           (printI (Right 3), 2, Nothing)]
--                 (eis, rs') = RS.promote (const True) rs
--
--                 expExec    = [(move 0 5, 0, Nothing), (printI 3, 2, Nothing)] :: [EPipeInstr]
--                 expRs      = RS.fromList [(mult 1 (Right 3) (Left 0), 1, Nothing)]
--
--             eis `shouldBe` expExec
--             rs' `shouldBe` expRs
--
--         it "does not remove operands if the condition fails" $ do
--             let rs         = RS.fromList [(move 0 (Right 5), 0, Nothing),
--                                           (mult 1 (Right 3) (Left 0), 1, Nothing),
--                                           (printI (Right 3), 2, Nothing)]
--                 (eis, rs') = RS.promote (const False) rs
--
--             eis `shouldBe` []
--             rs' `shouldBe` rs
