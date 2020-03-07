module Main where

import qualified Cli(opts, Args(..))
import Lib(getREPL)
import qualified REPL.REPL as REPL(loop)
import REPL.Lang

import System.Console.Haskeline(runInputT, defaultSettings)

import Options.Applicative

main :: IO ()
main = do
  args <- execParser Cli.opts

  let repl = getREPL (Cli.lang args)

  case Cli.file args of
    Just filename -> fromFile repl filename
    Nothing -> runInputT defaultSettings $ REPL.loop repl

fromFile readEval filename = do
  content <- readFile filename
  -- TODO removing empty lines should not be done here
  fromFileHelper readEval $ filter (/= "") $ lines content

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
