module REPL.REPL (loop) where

import System.Console.Haskeline
import REPL.Command
import REPL.Lang

prompt = "> "

loop :: REPL -> InputT IO ()
loop repl = do
  minput <- getInputLine prompt
  case minput of
    Nothing -> return ()
    Just input ->
      case parseCommand input of
        Left _ ->
          case readEvalPrint repl input of
            Left err -> do
              outputStrLn "Error"
              outputStrLn err
              loop repl
            Right (out, repl') -> do
              outputStrLn out
              loop repl'
        Right cmd -> do
          case cmd of
            Quit -> return ()
            Env -> outputStrLn $ env repl
            (LoadFile filename) -> outputStrLn $ "Command LoadFile " <> filename
          loop repl
