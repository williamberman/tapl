module Arith.Syntax (Term(..), isNumeric, isVal) where

import Arith.Parser(ParseTerm(..), ParseData)

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
  makeTrue _ = TTrue
  makeFalse _ = TFalse
  makeIf _ = If
  makeZero _ = Zero
  makeSucc _ = Succ
  makePred _ = Pred
  makeIsZero _ = IsZero

  intToTerm d n =
    if n <= 0 then
      Zero
    else
      Succ $ intToTerm d $ n - 1

isNumeric :: Term -> Bool

isNumeric Zero = True
isNumeric (Succ term) = isNumeric term
isNumeric (Pred term) = isNumeric term
isNumeric _ = False


isVal :: Term -> Bool

isVal TTrue = True
isVal TFalse = False
isVal term = isNumeric term

data AugTerm = AugTerm ParseData Term

instance ParseTerm AugTerm where
  makeTrue d = AugTerm d TTrue
  makeFalse d = AugTerm d TFalse
  makeIf d (AugTerm _ predicate) (AugTerm _ consequent) (AugTerm _ alternative) = AugTerm d (If predicate consequent alternative)
  makeZero d = AugTerm d Zero
  makeSucc d (AugTerm _ t) = AugTerm d (Succ t)
  makePred d (AugTerm _ t) = AugTerm d (Pred t)
  makeIsZero d (AugTerm _ t) = AugTerm d (IsZero t)

  intToTerm d n =
    if n <= 0 then
      AugTerm d Zero
    else
      AugTerm d $ Succ $ intToTerm d $ n - 1
