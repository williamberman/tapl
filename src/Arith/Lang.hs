module Arith.Lang where

import Arith.Parser(parseLine)
import qualified Arith.Semantics as Semantics(eval)
import Arith.Syntax(Term)

read :: String -> Either String Term
read line =
  case parseLine line of
    Left err -> Left $ show err
    Right term -> Right term

eval :: Term -> Either String String
eval term =
  case Semantics.eval term of
    Left err -> Left $ show err
    Right out -> Right $ show out
