module ParserSpec (parserSpec) where

import Test.Hspec
import Data.ByteString
import Parser
import Instr

parserSpec :: Spec
parserSpec = describe "parser" $ do
    word8Spec
    word32Spec
    byteSpec
    instrSpec
    instrsSpec

word8Spec :: Spec
word8Spec = describe "word8" $ do
    it "parses first byte from string" $ do
        let bs = pack [1, 2, 3]
        parse word8 bs `shouldBe` Just 1

    it "fails if empty" $ do
        parse word8 empty `shouldBe` Nothing

word32Spec :: Spec
word32Spec = describe "word32" $ do
    it "parses first 4 bytes into word32" $ do
        let bs = pack [0, 0, 1, 1]
        parse word32 bs `shouldBe` Just 257

    it "fails if less than 4 bytes remaining" $ do
        let bs = pack [0, 0, 1]
        parse word32 bs `shouldBe` Nothing

byteSpec :: Spec
byteSpec = describe "byte" $ do
    it "parses if the first byte matches" $ do
        let bs = pack [2, 3, 4]
        parse (byte 2) bs `shouldBe` Just 2

    it "fails if the first bytes does not match" $ do
        let bs = pack [2, 3, 4]
        parse (byte 1) bs `shouldBe` Nothing

instrSpec :: Spec
instrSpec = describe "instr" $ do
    it "parses MoveI" $ do
        let bs = pack [0,
                       2,          -- reg idx
                       0, 0, 0, 3] -- val
        parse instr bs `shouldBe` Just (MoveI 2 3)

    it "parses LoadIdx" $ do
        let bs = pack [1,
                       2,          -- reg idx
                       4,          -- base idx
                       0, 0, 0, 6] -- offset
        parse instr bs `shouldBe` Just (LoadIdx 2 4 6)

    it "parses LoadBaseIdx" $ do
        let bs = pack [2,
                       2, -- reg idx
                       4, -- base idx
                       6] -- offset idx
        parse instr bs `shouldBe` Just (LoadBaseIdx 2 4 6)

    it "parses StoreIdx" $ do
        let bs = pack [3,
                       2,          -- reg idx
                       4,          -- base idx
                       0, 0, 0, 6] -- offset
        parse instr bs `shouldBe` Just (StoreIdx 2 4 6)

    it "parses StoreBaseIdx" $ do
        let bs = pack [4,
                       2, -- reg idx
                       4, -- base idx
                       6] -- offset idx
        parse instr bs `shouldBe` Just (StoreBaseIdx 2 4 6)

    it "parses Add" $ do
        let bs = pack [5,
                       2, -- reg idx
                       4, -- x idx
                       6] -- y idx
        parse instr bs `shouldBe` Just (Add 2 4 6)

    it "parses AddI" $ do
        let bs = pack [6,
                       2,          -- reg idx
                       4,          -- x idx
                       0, 0, 0, 6] -- y operand
        parse instr bs `shouldBe` Just (AddI 2 4 6)

    it "parses Sub" $ do
        let bs = pack [7,
                       2, -- reg idx
                       4, -- x idx
                       6] -- y idx
        parse instr bs `shouldBe` Just (Sub 2 4 6)

    it "parses SubI" $ do
        let bs = pack [8,
                       2,          -- reg idx
                       4,          -- x idx
                       0, 0, 0, 6] -- y operand
        parse instr bs `shouldBe` Just (SubI 2 4 6)

    it "parses B" $ do
        let bs = pack [9,
                       0, 0, 0, 5] -- branch address
        parse instr bs `shouldBe` Just (B 5)

    it "parses BGT" $ do
        let bs = pack [10,
                       2,          -- reg idx
                       0, 0, 0, 6] -- branch address
        parse instr bs `shouldBe` Just (BGT 2 6)

    it "parses Ret" $ do
        let bs = pack [11]
        parse instr bs `shouldBe` Just Ret

    it "parses Print" $ do
        let bs = pack [12, 3]
        parse instr bs `shouldBe` Just (Print 3)

instrsSpec :: Spec
instrsSpec = describe "instrs" $ do
    it "parses many instructions" $ do
        let bs = pack [6,          -- Add
                       2,          -- reg idx
                       4,          -- x idx
                       0, 0, 0, 6, -- y operand
                       9,          -- B
                       0, 0, 0, 2] -- branch address
        parse instrs bs `shouldBe` Just [AddI 2 4 6, B 2]
