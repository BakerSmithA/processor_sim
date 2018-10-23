module Main where

import Test.Hspec
import ParserSpec
import VMSpec

main :: IO ()
main = hspec specs where
    specs = do
        parserSpec
        vmSpec
