module Lib (getREPL, LangSelector(..)) where

import qualified Arith.Lang as Arith(irepl, initialState, printState)
import qualified Untyped.Lang as Untyped(irepl, initialState, printState)
import REPL(REPL, makeREPL)

data LangSelector =
  Arith
  | Untyped
  deriving (Read, Show)

getREPL :: LangSelector -> REPL
getREPL Arith = makeREPL Arith.irepl Arith.initialState Arith.printState
getREPL Untyped = makeREPL Untyped.irepl Untyped.initialState Untyped.printState
