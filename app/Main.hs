module Main (main) where

import Lib
import System.IO as SysIO

main :: IO ()
main = do
  SysIO.hSetBuffering SysIO.stdout SysIO.NoBuffering
  repl

replPrompt :: String
replPrompt = "REPL> "

-- REPLのメインループ
repl :: IO ()
repl = do
    putStr replPrompt
    input <- getLine
    if input == "quit"
        then putStrLn "Bye!"
        else do
            putStrLn input
            repl
