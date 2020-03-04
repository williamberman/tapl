module Common.Semantics (EvalError(..), EvalStepResult(..), andThen) where

data EvalError a = EvalError
  { t :: a
  , message :: String
  }
  deriving Show

data EvalStepResult a =
  Error (EvalError a)
  | Continue a
  | Stop a

-- TODO pull this into a better abstraction
andThen :: (a -> EvalStepResult a) -> EvalStepResult a -> EvalStepResult a
andThen _ (Error err) = Error err
andThen f (Stop term) = f term
andThen f (Continue term) = f term
