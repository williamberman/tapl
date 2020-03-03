module Main where

import qualified Cli(opts, Args(..))
import Lib(getLang)

import Options.Applicative

main :: IO ()
main = do
  args <- execParser Cli.opts
  let lang = getLang (Cli.lang args)
  
  case Cli.file args of
    Just filename -> fromFile lang filename
    Nothing -> readEvalPrintLoop lang

fromFile readEval filename = do
  content <- readFile filename
  fromFileHelper readEval $ lines content

fromFileHelper readEval [] = return ()
fromFileHelper readEval (line : lines) =
    case readEval line of
      Left err -> do
        putStrLn "Error"
        putStrLn err
      Right res -> do
        putStrLn res
        fromFileHelper readEval lines

readEvalPrintLoop readEval = do
  let loop = do
        readEvalPrint readEval
        loop
  loop

readEvalPrint :: (String -> Either String String) -> IO ()
readEvalPrint readEval = do
  line <- getLine

  case readEval line of
    Left err -> do
      putStrLn "Error"
      putStrLn err
    Right res -> putStrLn res



