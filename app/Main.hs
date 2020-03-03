module Main where

import Lang(Lang, readEval)

import qualified Arith.Lang as Arith(lang)
import qualified Untyped.Lang as Untyped(lang)

main :: IO ()
main = do
  let lang = getLang Arith

  let loop = do {
    readEvalPrint $ readEval lang;
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


data LangSelector =
  Arith
  | Untyped

getLang :: LangSelector -> Lang a
getLang Arith = Arith.lang
getLang Untyped = Untyped.lang
