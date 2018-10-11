module Main where

import qualified Mem as Mem
import qualified Reg as Reg
import Instr
import VM as VM
import qualified Parser as P
import qualified Data.ByteString as B
import System.Environment

makeVm :: [Instr] -> VM
makeVm instrs = VM mem regs instrs' pcIdx spIdx lrIdx where
    mem = Mem.zeroed 1
    regs = Reg.file 8
    instrs' = Mem.fromList instrs
    pcIdx = 6
    spIdx = 7
    lrIdx = 8

runVm :: [Instr] -> VM
runVm = run . makeVm

runBytecode :: FilePath -> IO ()
runBytecode path = do
    contents <- B.readFile path
    case P.parse P.instrs contents of
        Nothing -> putStrLn "Could not parse file"
        Just is -> putStrLn (show $ runVm is)

main :: IO ()
main = do
    args <- getArgs
    let usageMsg = "Useage example: vm <bytecode_file>"
    case args of
        [path] -> runBytecode path
        _      -> error usageMsg
