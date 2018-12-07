module Main where

import Instr
import State
import CPU (run)
import qualified Parser as P
import qualified Data.ByteString as B
import System.Environment

runVm :: [FInstr] -> IO ()
runVm []     = putStrLn "No instructions to run"
runVm instrs = do
    let vm = run (State.emptyDefault instrs)
    putStrLn $ State.output vm
    putStrLn (show vm)

newlines :: FInstr -> String
newlines (Ret)     = "\n"
newlines (SysCall) = "\n"
newlines _         = ""

runBytecode :: FilePath -> IO ()
runBytecode path = do
    contents <- B.readFile path
    case P.parse P.instrs contents of
        Nothing -> putStrLn "Could not parse file"
        Just is -> runVm is

main :: IO ()
main = do
    args <- getArgs
    let usageMsg = "Useage example: vm <bytecode_file>"
    case args of
        [path] -> runBytecode path
        _      -> error usageMsg
