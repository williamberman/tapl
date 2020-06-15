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
  , setForm
  ) where

import qualified Common.Utils    as Utils
import           Data.List
import qualified Data.Map.Strict as Map

type DBIndex = Integer

type VariableIndices = Map.Map String DBIndex

data Environment = Environment
    { globals :: VariableIndices
    , locals  :: VariableIndices
    }
    deriving Show

makeEnvironment :: Environment
makeEnvironment = Environment {globals = Map.empty, locals = Map.empty}

data LookUpResult = LookUpResult
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

newtype State form = State (Map.Map String (DBIndex, Maybe form))

instance Show f => Show (State f) where
  show (State state) =
    intercalate "\n" $ map printAState $ Map.toList state
    where
      printAState :: Show f => (String, (DBIndex, Maybe f)) -> String
      printAState (name, (_, Nothing))   = name <> " is undefined"
      printAState (name, (_, Just form)) = name <> " = " <> show form

makeState :: Environment -> State f -> State f
makeState Environment {globals = globals'} (State state) =
  State $ Map.union state $ Map.fromList $ map (\(name, idx) -> (name, (idx, getForm name))) $ Map.assocs globals'
  where
    getForm name =
      case Map.lookup name state of
        Nothing        -> Nothing
        Just (_, form) -> form

initialState :: State f
initialState = State Map.empty

stateToEnvironment :: State f -> Environment
stateToEnvironment (State state) =
  Environment { globals = globals', locals = Map.empty }
  where
    globals' = Map.map fst state

setForm :: String -> f -> State f -> State f
setForm name form (State state) = State $ Map.insertWith (\new old -> (fst old, snd new)) name (0, Just form) state
