module Main where

import Test.Hspec
import VMSpec
import ParserSpec

main :: IO ()
main = hspec specs where
    specs = do
        vmSpec
        parserSpec
