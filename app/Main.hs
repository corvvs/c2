module Main (main) where

import Lexer
import Parser
import MyPrint
import System.IO as SysIO
import qualified Data.Text as T
import Control.Monad.Except
import qualified Data.Text.IO as TIO
import Control.Monad.IO.Class (liftIO)

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
    let input' = T.strip $ T.pack input
    if T.null input'
      then return ()
    else evaluate input'
    repl

evaluate :: T.Text -> IO ()
evaluate expression = do -- IOコンテキスト
  result <- runExceptT $ do -- ExceptT(ExceptTT)コンテキスト

    tokens <- lexer expression
    liftIO $ MyPrint.printLine "Tokens" $ T.pack $ show tokens

    ast <- parseLine expression tokens
    liftIO $ MyPrint.printLine "AST" $ T.pack $ show ast

  case result of
      Left err -> do
        TIO.putStrLn $ err
        return ()
      Right _ -> return ()
