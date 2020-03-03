module Untyped.Syntax (Term(..), substitute) where

type DBIndex = Integer

data Term =
  Abstraction Term
  | Application Term Term
  | Variable DBIndex
  deriving Show
  
  
substitute :: DBIndex -> Term -> Term -> Term

substitute idx replaceWith (Abstraction t) =
  Abstraction $ substitute (idx + 1) (shift 1 0 replaceWith) t

substitute idx replaceWith (Application t1 t2) =
  Application (substitute idx replaceWith t1) (substitute idx replaceWith t2)

substitute idx replaceWith (Variable replacingIdx) =
  Variable $ if idx == replacingIdx then idx else replacingIdx


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
