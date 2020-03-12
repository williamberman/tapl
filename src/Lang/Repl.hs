module Lang.Repl
  ( loop
  ) where

import           Lang.Command
import qualified Lang.File                as File (readFile)
import           Lang.Lang                (Lang, env, readEvalPrint)
import           System.Console.Haskeline

import           Control.Monad.IO.Class   (liftIO)
import qualified Data.Foldable

prompt = "> "

loop :: Lang -> InputT IO ()
loop lang = do
  minput <- getInputLine prompt
  case minput of
    Nothing -> return ()
    Just input ->
      case parseCommand input of
        Left _ -> do
          lang' <- runInput lang input
          loop lang'
        Right cmd -> do
          out <- runCommand lang cmd
          Data.Foldable.forM_ out loop

runInput :: Lang -> String -> InputT IO Lang
runInput lang input =
  case readEvalPrint lang input of
    Left err -> do
      outputStrLn "Error"
      outputStrLn err
      return lang
    Right (out, lang') -> do
      outputStrLn out
      return lang'

runCommand :: Lang -> Command -> InputT IO (Maybe Lang)
runCommand lang cmd =
  case cmd of
    Quit -> return Nothing
    Env -> do
      outputStrLn $ env lang
      return $ Just lang
    (LoadFile filename) -> loadFile lang filename

loadFile :: Lang -> String -> InputT IO (Maybe Lang)
loadFile lang filename = do
  contents <- liftIO $ readFile filename
  case File.readFile lang contents of
    Left err -> do
      outputStrLn "Error"
      outputStrLn err
      return $ Just lang
    Right (lang', out) ->
      case out of
        Just out' -> do
          outputStrLn out'
          return $ Just lang'
        Nothing -> return $ Just lang'
