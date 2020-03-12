module Lang.Repl (loop) where

import System.Console.Haskeline
import Lang.Command
import Lang.Lang(Lang, readEvalPrint, env)
import qualified Lang.File as File(readFile)

import Control.Monad.IO.Class(liftIO)

prompt = "> "

-- TODO break this function up
loop :: Lang -> InputT IO ()
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
            (LoadFile filename) -> do
              contents <- liftIO $ readFile filename
              case File.readFile repl contents of
                Left err -> do
                  outputStrLn "Error"
                  outputStrLn err
                  loop repl
                Right (repl', Nothing) -> loop repl'
                Right (repl', Just out) -> do
                  outputStrLn out
                  loop repl'
          loop repl
