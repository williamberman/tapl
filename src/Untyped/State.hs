module Untyped.State
  ( makeEnvironment
  , LookUpResult(..)
  , lookUp
  , addGlobal
  , addLocal
  , DBIndex
  , Environment
  , State
  , makeState
  , removeLocal
  , initialState
  ) where

import qualified Common.Utils    as Utils
import qualified Data.Map.Strict as Map

type DBIndex = Integer

type VariableIndices = Map.Map String DBIndex

data Environment =
  Environment
    { globals :: VariableIndices
    , locals  :: VariableIndices
    }

makeEnvironment :: Environment
makeEnvironment = Environment {globals = Map.empty, locals = Map.empty}

data LookUpResult =
  LookUpResult
    { global :: Maybe DBIndex
    , local  :: Maybe DBIndex
    }

lookUp :: String -> Environment -> LookUpResult
lookUp name Environment {globals = globals', locals = locals'} =
  LookUpResult {global = globals' Map.!? name, local = locals' Map.!? name}

addGlobal :: String -> Environment -> (DBIndex, Environment)
addGlobal name Environment {globals = globals', locals = locals'} =
  (newGlobalIdx, Environment {globals = globals'', locals = locals'})
  where
    newGlobalIdx = (+ 1) $ Utils.safeMaximum (Utils.safeMaximum 0 $ Map.elems locals') $ Map.elems globals'
    globals'' = Map.insert name newGlobalIdx globals'

addLocal :: String -> Environment -> Environment
addLocal name Environment {globals = globals', locals = locals'} = Environment {globals = globals'', locals = locals''}
  where
    locals'' = Map.insert name 0 $ Map.map (+ 1) locals'
    globals'' = Map.map (+ 1) globals'

removeLocal :: String -> Environment -> Environment
removeLocal name Environment {globals = globals', locals = locals'} =
  Environment {globals = globals'', locals = locals''}
  where
    locals'' = Map.delete name $ Map.map (1 -) locals'
    globals'' = Map.map (1 -) globals'

type State form = Map.Map String (DBIndex, Maybe form)

makeState :: Environment -> State f -> State f
makeState Environment{globals=globals'} state =
  Map.fromList $ map (\(name, idx) -> (name, (idx, getForm name))) $ Map.assocs globals'
  where
    getForm = (\name -> case Map.lookup name state of
      Nothing -> Nothing
      Just (_, form) -> form)

initialState :: State f
initialState = Map.empty

stateToEnvironment :: State f -> Environment
stateToEnvironment state =
  Environment { globals = globals', locals = Map.empty }
  where
    globals' = Map.map fst state
