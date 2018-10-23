module Main where

import Data.Word (Word32)
import qualified Mem as Mem
import Instr
import VM as VM
import State
import qualified Parser as P
import qualified Data.ByteString as B
import System.Environment
--
-- makeVm :: [Instr] -> VM
-- makeVm instrs = VM mem regs instrs' pcIdx spIdx lrIdx bpIdx retIdx [] where
--     mem = Mem.zeroed 16
--     regs = Reg.file 17
--     instrs' = Mem.fromList instrs
--     pcIdx  = 13
--     spIdx  = 14
--     lrIdx  = 15
--     bpIdx  = 16
--     retIdx = 17
--
-- runVm :: [Instr] -> IO ()
-- runVm []     = putStrLn "No instructions to run"
-- runVm instrs = putStrLn $ VM.output $ run (makeVm instrs)
-- --putStrLn $ unlines $ fmap show (runAll (makeVm instrs))
--
-- newlines :: Instr -> String
-- newlines (Ret) = "\n"
-- newlines _     = ""
--
-- showInstrs :: [Instr] -> String
-- showInstrs is = unlines strNumbered where
--     strNumbered = map (\(n, i) -> (show n) ++ ":\t" ++ (show i) ++ newlines i) numbered
--     numbered    = zip [0..] is
--
-- runBytecode :: FilePath -> IO ()
-- runBytecode path = do
--     contents <- B.readFile path
--     case P.parse P.instrs contents of
--         Nothing -> putStrLn "Could not parse file"
--         Just is -> do
--             putStrLn (showInstrs is)
--             runVm is
--
-- main :: IO ()
-- main = do
--     args <- getArgs
--     let usageMsg = "Useage example: vm <bytecode_file>"
--     case args of
--         [path] -> runBytecode path
--         _      -> error usageMsg

main :: IO ()
main = do
    let mem = Mem.fromList [1]
    let regs = Mem.fromList [1]
    let is  = Mem.fromList [] :: Mem.Mem Word32 Instr
    let st = State mem regs is 0 0 0 0 0 []
    putStrLn (show mem)
    putStrLn (show regs)
    putStrLn (show is)
    putStrLn (show st)
