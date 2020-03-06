module Untyped.Syntax (Term(..), makeReplacement, substitute, shift, applyIndices) where

import Untyped.Syntax0 (Term0(..))
import Untyped.State

data Term =
  Abstraction Term
  | Application Term Term
  | Variable DBIndex
  deriving (Show, Eq)


-- applyIndices

applyIndices :: Term0 -> (Term, Environment)
applyIndices term =
  applyIndices' makeEnvironment term

applyIndices' :: Environment -> Term0 -> (Term, Environment)

applyIndices' env (Variable0 name) =
  case lookUp name env of
    LookUpResult{local = Just localIdx} -> (Variable localIdx, env)
    LookUpResult{global = Just globalIdx} -> (Variable globalIdx, env)
    _ -> (Variable newGlobalIdx, env')
  where
    (newGlobalIdx, env')  = addGlobal name env

applyIndices' env (Application0 t1 t2) =
  (Application t1' t2', env'')
  where
    (t1', env') = applyIndices' env t1
    (t2', env'') = applyIndices' env' t2

applyIndices' env (Abstraction0 name term) = (Abstraction term', env'')
  where
    (term', env') = applyIndices' (addLocal name env) term
    env'' = removeLocal name env'

    
 -- substitute

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


-- shift

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
