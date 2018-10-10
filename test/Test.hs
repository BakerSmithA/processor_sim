module Main where

import Test.Hspec
import VMSpec

main :: IO ()
main = hspec specs where
    specs = do
        vmSpec
