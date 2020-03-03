module Main where

import Lib(LangSelector(..), getLang)

main :: IO ()
main = do
  let lang = getLang Arith

  let loop = do {
    readEvalPrint $ lang;
    loop
  }

  loop

readEvalPrint :: (String -> Either String String) -> IO ()
readEvalPrint readEval = do
  line <- getLine

  case readEval line of
    Left err -> do
      putStrLn "Error"
      putStrLn err
    Right res -> putStrLn res


