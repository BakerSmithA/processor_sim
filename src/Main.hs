module Main where

import Instr
import State
import CPU (run)
import qualified Parser as P
import qualified Data.ByteString as B
import System.Environment

newlines :: FInstr -> String
newlines (Branch (Ret _)) = "\n"
newlines (Branch SysCall) = "\n"
newlines _                = ""

showInstrs :: [FInstr] -> String
showInstrs is = unlines strNumbered where
    strNumbered = map (\(n, i) -> (show n) ++ ":\t" ++ (show i) ++ newlines i) numbered
    numbered    = zip [0..] is

runVm :: [FInstr] -> IO ()
runVm []     = putStrLn "No instructions to run"
runVm instrs = do
    let vm = run (State.emptyDefault instrs)
    putStrLn $ showInstrs instrs
    putStrLn "\n"
    putStrLn $ State.output vm
    putStrLn (show vm)

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
