module ParserSpec (parserSpec) where

import Test.Hspec
import Data.ByteString
import Parser

parserSpec :: Spec
parserSpec = describe "parser" $ do
    word8Spec
    word32Spec

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
