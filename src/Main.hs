module Main where

import Data.Word (Word32)
import qualified Mem as Mem
import Instr
import State as State
import Res as Res
import State
import Bypass as BP
import qualified Parser as P
import qualified Data.ByteString as B
import System.Environment

runVm :: [Instr] -> IO ()
runVm []     = putStrLn "No instructions to run"
runVm instrs = do
    let vm = run (State.emptyDefault instrs)
    putStrLn $ State.output vm
    putStrLn (show vm)

newlines :: Instr -> String
newlines (Ret)     = "\n"
newlines (SysCall) = "\n"
newlines _         = ""

showInstrs :: [Instr] -> String
showInstrs is = unlines strNumbered where
    strNumbered = map (\(n, i) -> (show n) ++ ":\t" ++ (show i) ++ newlines i) numbered
    numbered    = zip [0..] is

runBytecode :: FilePath -> IO ()
runBytecode path = do
    contents <- B.readFile path
    case P.parse P.instrs contents of
        Nothing -> putStrLn "Could not parse file"
        Just is -> do
            putStrLn (showInstrs is)
            runVm is

main :: IO ()
main = do
    args <- getArgs
    let usageMsg = "Useage example: vm <bytecode_file>"
    case args of
        [path] -> runBytecode path
        _      -> error usageMsg
