module Lib (getLang, LangSelector(..)) where

import Lang(readEval)

import qualified Arith.Lang as Arith(lang)
import qualified Untyped.Lang as Untyped(lang)

data LangSelector =
  Arith
  | Untyped
  deriving (Read, Show)

getLang :: LangSelector -> String -> Either String String
getLang Arith = readEval Arith.lang
getLang Untyped = readEval Untyped.lang
