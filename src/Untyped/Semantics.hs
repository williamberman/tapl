module Untyped.Semantics (eval) where

import Untyped.Syntax(Term(..), makeReplacement, substitute, shift)

data EvalStepResult =
  Error EvalError
  | Continue Term
  | Stop Term

data EvalError = EvalError
  { t :: Term
  , message :: String
  }
  deriving (Show, Eq)

eval :: Term -> Either EvalError Term

eval term = case eval1 term of
  Error err -> Left err
  Continue next -> eval next
  Stop term' -> Right term'


-- Repetition in implementation ensures consistent reduction order
eval1 :: Term -> EvalStepResult

eval1 (Application (Abstraction t1) (Abstraction t2)) =
    Continue $ shift (-1) 0 $ substitute (makeReplacement 0 $ shift 1 0 $ Abstraction t2) t1

eval1 (Application t1 (Abstraction t2)) =
  andThen (\t1' -> Continue $ Application t1' $ Abstraction t2) (eval1 t1)

eval1 (Application (Abstraction t1) t2) =
  andThen (Continue . Application (Abstraction t1)) (eval1 t2)
  
eval1 (Application t1 t2) = 
  andThen (\t1' -> Continue $ Application t1' t2) (eval1 t1)

eval1 t = Stop t


-- TODO pull this into a better abstraction
andThen :: (Term -> EvalStepResult) ->  EvalStepResult -> EvalStepResult
andThen _ (Error err) = Error err
andThen f (Stop term) = f term
andThen f (Continue term) = f term

