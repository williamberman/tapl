module Arith.Termable (Termable(..)) where

class Termable a where
  makeTrue :: a
  makeFalse :: a
  makeIf :: a -> a -> a -> a
  makeZero :: a
  makeSucc :: a -> a
  makePred :: a -> a
  makeIsZero :: a -> a
  intToTerm :: Int -> a

