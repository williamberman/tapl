module Arith.Syntax (Term(..), isNumeric, isVal, intToTerm) where

data Term =
  TTrue
  | TFalse
  | If Term Term Term
  | Zero
  | Succ Term
  | Pred Term
  | IsZero Term
  deriving Show


isNumeric :: Term -> Bool

isNumeric Zero = True
isNumeric (Succ term) = isNumeric term
isNumeric (Pred term) = isNumeric term
isNumeric _ = False


isVal :: Term -> Bool

isVal TTrue = True
isVal TFalse = False
isVal term = isNumeric term

intToTerm :: Int -> Term
intToTerm n =
  if n <= 0 then
    Zero
  else
    Succ $ intToTerm $ n - 1
