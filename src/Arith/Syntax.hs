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
  makeIf pred' cons alt _ = If pred' cons alt
  makeZero _ = Zero
  makeSucc t _ = Succ t
  makePred t _ = Pred t
  makeIsZero t _ = IsZero t

  intToTerm n d =
    if n <= 0 then
      Zero
    else
      Succ $ intToTerm (n - 1) d

isNumeric :: Term -> Bool

isNumeric Zero = True
isNumeric (Succ term) = isNumeric term
isNumeric (Pred term) = isNumeric term
isNumeric _ = False


isVal :: Term -> Bool

isVal TTrue = True
isVal TFalse = False
isVal term = isNumeric term

data AugTerm = AugTerm Term ParseData

instance ParseTerm AugTerm where
  makeTrue = AugTerm TTrue
  makeFalse = AugTerm TFalse
  makeIf (AugTerm predicate _) (AugTerm consequent _) (AugTerm alternative _) = AugTerm (If predicate consequent alternative)
  makeZero = AugTerm Zero
  makeSucc (AugTerm t _) = AugTerm (Succ t)
  makePred (AugTerm t _) = AugTerm (Pred t)
  makeIsZero (AugTerm t _) = AugTerm (IsZero t)

  intToTerm n d =
    if n <= 0 then
      AugTerm Zero d
    else
      AugTerm (Succ $ intToTerm (n - 1) d) d
