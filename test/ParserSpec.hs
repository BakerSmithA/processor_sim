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
        let bs = pack [0, 0, 0, 0, 2, 0, 0, 0, 3]
        parse moveI bs `shouldBe` Just (MoveI 2 3)
