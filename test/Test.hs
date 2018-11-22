module Main where

import Test.Hspec
import ParserSpec
import QueueSpec
import RSSpec
-- import VMSpec

main :: IO ()
main = hspec specs where
    specs = do
        parserSpec
        queueSpec
        rsSpec
        -- vmSpec
