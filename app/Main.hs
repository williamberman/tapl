module Main where

import qualified Cli(opts, Args(..))
import Lib(getREPL)
import REPL(readEvalPrint)

import Options.Applicative

main :: IO ()
main = do
  args <- execParser Cli.opts

  let repl = getREPL (Cli.lang args)

  case Cli.file args of
    Just filename -> fromFile repl filename
    Nothing -> readEvalPrintLoop repl

fromFile readEval filename = do
  content <- readFile filename
  fromFileHelper readEval $ lines content

fromFileHelper repl [] = return ()
fromFileHelper repl (line : lines) = do
    putStrLn line
    case readEvalPrint repl line of
      Left err -> do
        putStrLn "Error"
        putStrLn err
      Right (out, repl') -> do
        putStrLn out
        fromFileHelper repl' lines

readEvalPrintLoop repl = do
  line <- getLine

  case readEvalPrint repl line of
    Left err -> do
      putStrLn "Error"
      putStrLn err
    Right (out, repl') -> do
      putStrLn out
      readEvalPrintLoop repl'



