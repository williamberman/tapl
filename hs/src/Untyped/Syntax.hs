module Untyped.Syntax
  ( Statement(..)
  , Assignment(..)
  , Term(..)
  , Variable(..)
  , makeReplacement
  , substitute
  , shift
  , applyIndices
  ) where

import           Common.Parser (ParseData)
import           Untyped.State

data Statement = StatementTerm Term
    | StatementAssignment Assignment
    | Nop
    deriving Show

data Assignment = Assignment String Term ParseData
    deriving Show

data Term = Abstraction
    { term      :: Term
    , name      :: String
    , parseData :: Maybe ParseData
    }
    | Application
    { t1        :: Term
    , t2        :: Term
    , parseData :: Maybe ParseData
    }
    | Variable
    { var       :: Variable
    , parseData :: Maybe ParseData
    }

-- TODO Is there a better way to do this?
data Variable = NamedVariable
    { variableName :: String
    }
    | NamelessVariable
    { idx          :: DBIndex
    , variableName :: String
    }

getIdx :: Variable -> DBIndex
getIdx NamelessVariable {idx = idx'} = idx'
getIdx NamedVariable {} = error "Attempted to get index of NamedVariable"

setIdx :: DBIndex -> Variable -> Variable
setIdx idx' var =
  case var of
    NamelessVariable {} -> var {idx = idx'}
    NamedVariable {variableName = variableName'} -> NamelessVariable {idx = idx', variableName = variableName'}

-- TODO this equality check is not necessarily "correct" in terms of checking if two terms are equal.
-- what does it really mean for two terms to be equal?
instance Eq Term where
  Abstraction {term = term', name = name'} == Abstraction {term = term'', name = name''} =
    (term' == term'') && (name' == name'')
  Application {t1 = t1', t2 = t2'} == Application {t1 = t1'', t2 = t2''} = (t1' == t1'') && (t2' == t2'')
  Variable{var = NamelessVariable{idx = idx', variableName = name'}} == Variable {var = NamelessVariable { idx = idx''
                                                                                                             , variableName = name''
                                                                                                             }} =
    (idx' == idx'') && (name' == name'')
  Variable{var = NamedVariable{variableName = name'}} == Variable{var = NamedVariable{variableName = name''}} =
    name' == name''
  _ == _ = False

instance Show Term where
  show Abstraction {term = term', name = name'} = "lambda " <> name' <> ". " <> show term'
  show Application {t1 = t1', t2 = t2'} =
    case t1' of
      Application {} -> "(" <> show t1' <> ") " <> show t2'
      _              -> show t1' <> " " <> show t2'
  show Variable{var = var'} = variableName var'

-- applyIndices
applyIndices :: Term -> (Term, Environment)
applyIndices = applyIndices' makeEnvironment

applyIndices' :: Environment -> Term -> (Term, Environment)
applyIndices' env Variable {var = var', parseData=parseData'} =
  case lookUp (variableName var') env of
    LookUpResult {local = Just localIdx} -> (Variable{var=setIdx localIdx var', parseData=parseData'}, env)
    LookUpResult {global = Just globalIdx} -> (Variable{var=setIdx globalIdx var', parseData=parseData'}, env)
    _ -> (Variable{var=setIdx newGlobalIdx var', parseData=parseData'}, env')
  where
    (newGlobalIdx, env') = addGlobal (variableName var') env
applyIndices' env Application {t1 = t1', t2 = t2', parseData = parseData'} =
  (Application {t1 = t1'', t2 = t2'', parseData = parseData'}, env'')
  where
    (t1'', env') = applyIndices' env t1'
    (t2'', env'') = applyIndices' env' t2'
applyIndices' env Abstraction {name = name', term = term', parseData = parseData'} =
  (Abstraction {term = term'', name = name', parseData = parseData'}, env'')
  where
    (term'', env') = applyIndices' (addLocal name' env) term'
    env'' = removeLocal name' env'

-- substitute
data Replacement = Replacement DBIndex Term

makeReplacement :: DBIndex -> Term -> Replacement
makeReplacement = Replacement

substitute :: Replacement -> Term -> Term
substitute (Replacement idx replaceWith) Abstraction {term = term', name = name', parseData = parseData'} =
  Abstraction
    {term = substitute (Replacement (idx + 1) (shift 1 0 replaceWith)) term', name = name', parseData = parseData'}
substitute replacement Application {t1 = t1', t2 = t2', parseData = parseData'} =
  Application {t1 = substitute replacement t1', t2 = substitute replacement t2', parseData = parseData'}
substitute (Replacement idx replaceWith) Variable{var = var', parseData = parseData'} =
  if idx == replacingIdx
    then replaceWith
    else Variable{var = var', parseData = parseData'}
  where
    replacingIdx = getIdx var'

-- shift
shift :: DBIndex -> DBIndex -> Term -> Term
shift increaseBy numBinders Abstraction {term = term', name = name', parseData = parseData'} =
  Abstraction {term = shift increaseBy (numBinders + 1) term', name = name', parseData = parseData'}
shift increaseBy numBinders Application {t1 = t1', t2 = t2', parseData = parseData'} =
  Application {t1 = shift increaseBy numBinders t1', t2 = shift increaseBy numBinders t2', parseData = parseData'}
shift increaseBy numBinders Variable {var = var', parseData = parseData'} =
  Variable {var = setIdx idx'' var', parseData = parseData'}
  where
    idx' = getIdx var'
    idx'' =
      if isFree numBinders idx'
        then increaseBy + idx'
        else idx'
-- A free variable's index reaches outside the number of binders present for it
isFree :: DBIndex -> DBIndex -> Bool
isFree numBinders idx = idx >= numBinders
