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
        parse instr bs `shouldBe` Just (moveI 2 3)

    it "parses Move" $ do
        let bs = pack [14,
                       2, -- reg idx
                       3] -- from idx
        parse instr bs `shouldBe` Just (move 2 3)

    it "parses LoadIdx" $ do
        let bs = pack [1,
                       2,          -- reg idx
                       4,          -- base idx
                       0, 0, 0, 6] -- offset
        parse instr bs `shouldBe` Just (loadIdx 2 4 6)

    it "parses LoadBaseIdx" $ do
        let bs = pack [2,
                       2, -- reg idx
                       4, -- base idx
                       6] -- offset idx
        parse instr bs `shouldBe` Just (loadBaseIdx 2 4 6)

    it "parses StoreIdx" $ do
        let bs = pack [3,
                       2,          -- reg idx
                       4,          -- base idx
                       0, 0, 0, 6] -- offset
        parse instr bs `shouldBe` Just (storeIdx 2 4 6)

    it "parses StoreBaseIdx" $ do
        let bs = pack [4,
                       2, -- reg idx
                       4, -- base idx
                       6] -- offset idx
        parse instr bs `shouldBe` Just (storeBaseIdx 2 4 6)

    it "parses Add" $ do
        let bs = pack [5,
                       2, -- reg idx
                       4, -- x idx
                       6] -- y idx
        parse instr bs `shouldBe` Just (add 2 4 6)

    it "parses AddI" $ do
        let bs = pack [16,
                       2, -- reg idx
                       4, -- x idx
                       0, 0, 0, 6] -- y idx
        parse instr bs `shouldBe` Just (addI 2 4 6)

    it "parses Sub" $ do
        let bs = pack [6,
                       2, -- reg idx
                       4, -- x idx
                       6] -- y idx
        parse instr bs `shouldBe` Just (sub 2 4 6)

    it "parses SubI" $ do
        let bs = pack [17,
                       2, -- reg idx
                       4, -- x idx
                       0, 0, 0, 6] -- y idx
        parse instr bs `shouldBe` Just (subI 2 4 6)

    it "parses Mult" $ do
        let bs = pack [18,
                       2, -- reg idx
                       4, -- x idx
                       6] -- y idx
        parse instr bs `shouldBe` Just (mult 2 4 6)

    it "parses Eq" $ do
        let bs = pack [7,
                       2, -- reg idx
                       4, -- x idx
                       6] -- y idx
        parse instr bs `shouldBe` Just (eq 2 4 6)

    it "parses Lt" $ do
        let bs = pack [19,
                       2, -- reg idx
                       4, -- x idx
                       6] -- y idx
        parse instr bs `shouldBe` Just (lt 2 4 6)

    it "parses Or" $ do
        let bs = pack [8,
                       2, -- reg idx
                       4, -- x idx
                       6] -- y idx
        parse instr bs `shouldBe` Just (orI 2 4 6)

    it "parses And" $ do
        let bs = pack [9,
                       2, -- reg idx
                       4, -- x idx
                       6] -- y idx
        parse instr bs `shouldBe` Just (andI 2 4 6)

    it "parses Not" $ do
        let bs = pack [15,
                       2, -- reg idx
                       4] -- x idx
        parse instr bs `shouldBe` Just (notI 2 4)

    it "parses B" $ do
        let bs = pack [10,
                       0, 0, 0, 5] -- branch address
        parse instr bs `shouldBe` Just (b 5)

    it "parses BT" $ do
        let bs = pack [11,
                       2,          -- reg idx
                       0, 0, 0, 6] -- branch address
        parse instr bs `shouldBe` Just (bt 2 6)

    it "parses BF" $ do
        let bs = pack [22,
                       2,          -- reg idx
                       0, 0, 0, 6] -- branch address
        parse instr bs `shouldBe` Just (bf 2 6)

    it "parses Ret" $ do
        let bs = pack [12]
        parse instr bs `shouldBe` Just ret

    it "parses SysCall" $ do
        let bs = pack [21]
        parse instr bs `shouldBe` Just sysCall

    it "parses Print" $ do
        let bs = pack [13, 3]
        parse instr bs `shouldBe` Just (printI 3)

    it "parses PrintC" $ do
        let bs = pack [24, 3]
        parse instr bs `shouldBe` Just (printC 3)

    it "parses PrintLn" $ do
        let bs = pack [20]
        parse instr bs `shouldBe` Just printLn

instrsSpec :: Spec
instrsSpec = describe "instrs" $ do
    it "parses many instructions" $ do
        let bs = pack [0,0,0,0,0,5,14,1,0,13,1]
        parse instrs bs `shouldBe` Just [moveI 0 5, move 1 0, printI 1]
