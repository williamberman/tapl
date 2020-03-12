module Lib (getLang, LangSelector(..)) where

import qualified Arith.Lang as Arith(repl)
import qualified Untyped.Lang as Untyped(repl)
import Lang.Lang(Lang)

data LangSelector =
  Arith
  | Untyped
  deriving (Read, Show)

getLang :: LangSelector -> Lang
getLang Arith = Arith.repl
getLang Untyped = Untyped.repl
