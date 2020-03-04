module Untyped.Syntax (Term(..), makeReplacement, substitute, shift, applyIndices) where

import Untyped.Syntax0 (Term0(..))

import qualified Data.Map.Strict as Map
import qualified Common.Utils as Utils

type DBIndex = Integer

data Term =
  Abstraction Term
  | Application Term Term
  | Variable DBIndex
  deriving (Show, Eq)

type VariableIndices = Map.Map String DBIndex
data Environment = Environment
  { globals :: VariableIndices
  , locals :: VariableIndices
  }

applyIndices :: Term0 -> Term
applyIndices term =
    term'
  where
    (term', _) = applyIndices' (Environment{ globals = Map.empty, locals = Map.empty }) term

applyIndices' :: Environment -> Term0 -> (Term, VariableIndices)

applyIndices' Environment {globals = globals', locals = locals'} (Variable0 name) =
  case (locals' Map.!? name, globals' Map.!? name) of
    (Just localIdx, _) -> (Variable localIdx, globals')
    (_, Just globalIdx) -> (Variable globalIdx, globals')
    (_, _) -> (Variable newGlobalIdx, Map.insert name newGlobalIdx globals')
  where
    newGlobalIdx = (+ 1) $ Utils.safeMaximum $ Map.elems globals'

applyIndices' env (Application0 t1 t2) =
  (Application t1' t2', globals'')
  where
    (t1', globals') = applyIndices' env t1
    (t2', globals'') = applyIndices' (env { globals = globals'' }) t2

applyIndices' env (Abstraction0 name term) =
  (Abstraction term', newGlobals)
  where
    newLocals = Map.insert name 0 $ Map.map (+1) $ locals env
    (term', newGlobals) = applyIndices' (env { locals = newLocals }) term

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
