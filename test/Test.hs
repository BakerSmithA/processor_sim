module Main where

import Test.Hspec
import ParserSpec
import RRTSpec
import QueueSpec
-- import ROBSpec
-- import RSSpec
-- import BypassSpec
-- import CPUSpec

main :: IO ()
main = hspec specs where
    specs = do
        parserSpec
        rrtSpec
        queueSpec
        -- robSpec
        -- rsSpec
        -- bypassSpec
        -- cpuSpec
