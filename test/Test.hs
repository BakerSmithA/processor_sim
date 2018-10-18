module Main where

import Test.Hspec
import StateSpec
import ParserSpec
import VM

main :: IO ()
main = hspec specs where
    specs = do
        stateSpec
        parserSpec
