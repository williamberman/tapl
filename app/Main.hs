module Main where

import qualified Cli                      (Args (..), opts)

import qualified Lang.File                as File (readFile)
import qualified Lang.Repl                as Repl (loop)
import           Lib                      (getLang)

import           System.Console.Haskeline (defaultSettings, runInputT)

import           Options.Applicative

main :: IO ()
main = do
  args <- execParser Cli.opts

  let lang = getLang (Cli.lang args)

  case Cli.file args of
    Just filename -> readFileWrapper lang filename
    Nothing       -> runInputT defaultSettings $ Repl.loop lang

readFileWrapper lang filename = do
  content <- readFile filename
  case File.readFile lang content of
    Left err -> do
      putStrLn "Error"
      putStrLn err
    Right (_, out) -> putStrLn out
