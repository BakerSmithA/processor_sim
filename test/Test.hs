module Main where

import Test.Hspec
import ParserSpec
import ExecSpec

main :: IO ()
main = hspec specs where
    specs = do
        parserSpec
        execSpec
