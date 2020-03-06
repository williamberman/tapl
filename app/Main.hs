module Main where

import qualified Cli(opts, Args(..))
import Lib(getREPL)
import REPL(readEvalPrint, env)

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

  -- TODO should be some better way to enter a "command mode"
  if line == ":env" then do
    putStrLn $ env repl
    readEvalPrintLoop repl
  else
    case readEvalPrint repl line of
      Left err -> do
        putStrLn "Error"
        putStrLn err
        readEvalPrintLoop repl
      Right (out, repl') -> do
        putStrLn out
        readEvalPrintLoop repl'
