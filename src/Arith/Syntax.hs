module Arith.Syntax (Term(..), isNumeric, isVal) where

import Arith.Parser(ParseTerm(..))

data Term =
  TTrue
  | TFalse
  | If Term Term Term
  | Zero
  | Succ Term
  | Pred Term
  | IsZero Term
  deriving Show

instance ParseTerm Term where
  makeTrue = TTrue
  makeFalse = TFalse
  makeIf = If
  makeZero = Zero
  makeSucc = Succ
  makePred = Pred
  makeIsZero = IsZero

  intToTerm n =
    if n <= 0 then
      Zero
    else
      Succ $ intToTerm $ n - 1

isNumeric :: Term -> Bool

isNumeric Zero = True
isNumeric (Succ term) = isNumeric term
isNumeric (Pred term) = isNumeric term
isNumeric _ = False


isVal :: Term -> Bool

isVal TTrue = True
isVal TFalse = False
isVal term = isNumeric term

