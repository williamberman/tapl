module Untyped.Syntax (Term(..), makeReplacement, substitute, shift) where

type DBIndex = Integer

data Term =
  Abstraction Term
  | Application Term Term
  | Variable DBIndex
  deriving (Show, Eq)

data Replacement = Replacement DBIndex Term

makeReplacement :: DBIndex -> Term -> Replacement
makeReplacement = Replacement

substitute :: Replacement -> Term -> Term

substitute (Replacement idx replaceWith) (Abstraction t) =
  Abstraction $ substitute (Replacement (idx + 1) (shift 1 0 replaceWith)) t

substitute replacement (Application t1 t2) =
  Application (substitute replacement t1) (substitute replacement t2)

substitute (Replacement idx replaceWith) (Variable replacingIdx) =
  if idx == replacingIdx then replaceWith else Variable replacingIdx


shift :: DBIndex -> DBIndex -> Term -> Term

shift increaseBy numBinders (Abstraction t) =
  Abstraction $ shift increaseBy (numBinders + 1) t

shift increaseBy numBinders (Application t1 t2) =
  Application (shift increaseBy numBinders t1) (shift increaseBy numBinders t2)

shift increaseBy numBinders (Variable idx) =
  Variable $ if isFree numBinders idx then increaseBy + idx else idx


-- A free variable's index reaches outside the number of binders present for it
isFree :: DBIndex -> DBIndex -> Bool
isFree numBinders idx = idx >= numBinders
