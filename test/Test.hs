module Main where

import Test.Hspec
import ParserSpec
import RRTSpec
import QueueSpec
import ROBSpec
import ExecSpec

main :: IO ()
main = hspec specs where
    specs = do
        --parserSpec
        --rrtSpec
        queueSpec
        robSpec
        --execSpec
