module Main where

import Test.Hspec
import ParserSpec
import MemSpec
import RRTSpec
import QueueSpec
import ROBSpec
import RSSpec
import BypassSpec
import CPUSpec

main :: IO ()
main = hspec specs where
    specs = do
        -- parserSpec
        -- memSpec
        -- rrtSpec
        -- queueSpec
        -- robSpec
        -- rsSpec
        -- bypassSpec
        cpuSpec
