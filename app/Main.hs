module Main where

import qualified Arith.Lang as Arith(read, eval)

main :: IO ()
main = do
  readEvalPrintLoop Arith.read Arith.eval

readEvalPrintLoop :: (String -> Either String a) -> (a -> Either String String) -> IO ()
readEvalPrintLoop read eval = do
  line <- getLine

  let out = case read line of
        Left err -> Left err
        Right parsed -> eval parsed

  case out of
    Left err -> do
      putStrLn "Error"
      putStrLn err
    Right res -> putStrLn res

  readEvalPrintLoop read eval
