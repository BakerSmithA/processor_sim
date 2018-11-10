module Main where

import Data.Word (Word32)
import qualified Mem as Mem
import Instr
import State as State
import VM as VM
import State
import Bypass as BP
import qualified Parser as P
import qualified Data.ByteString as B
import System.Environment

makeVm :: [Instr] -> State
makeVm instrs = State mem regs instrs' pcIdx spIdx lrIdx bpIdx retIdx [] BP.empty where
    mem = Mem.zeroed 32
    regs = Mem.zeroed 17
    instrs' = Mem.fromList instrs
    pcIdx  = 13
    spIdx  = 14
    lrIdx  = 15
    bpIdx  = 16
    retIdx = 17

runVm :: [Instr] -> IO ()
runVm []     = putStrLn "No instructions to run"
runVm instrs = do
    let vm = run (makeVm instrs)
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
