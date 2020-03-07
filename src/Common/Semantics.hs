module Common.Semantics (EvalError(..), EvalStepResult(..), andThen, stopIfSame) where

data EvalError a = EvalError
  { t :: a
  , message :: String
  }
  deriving Show

data EvalStepResult a =
  Error (EvalError a)
  | Continue a
  | Stop a
  deriving Show

-- TODO pull this into a better abstraction
andThen :: (a -> EvalStepResult a) -> EvalStepResult a -> EvalStepResult a
andThen _ (Error err) = Error err
andThen f (Stop term) = f term
andThen f (Continue term) = f term

stopIfSame :: Eq a => (a -> EvalStepResult a) -> a -> EvalStepResult a
stopIfSame f input =
  case output of
    (Continue output') -> if input == output' then Stop input else output
    _ -> output
  where
    output = f input
