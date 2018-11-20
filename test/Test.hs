module Main where

import Test.Hspec
import ParserSpec
import QueueSpec
import VMSpec

main :: IO ()
main = hspec specs where
    specs = do
        parserSpec
        queueSpec
        vmSpec
