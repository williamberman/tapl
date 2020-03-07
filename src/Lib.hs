module Lib (getREPL, LangSelector(..)) where

import qualified Arith.Lang as Arith(repl)
import qualified Untyped.Lang as Untyped(repl)
import REPL.Lang(REPL, makeREPL)

data LangSelector =
  Arith
  | Untyped
  deriving (Read, Show)

getREPL :: LangSelector -> REPL
getREPL Arith = Arith.repl
getREPL Untyped = Untyped.repl
