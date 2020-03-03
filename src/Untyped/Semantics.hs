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


eval1 :: Term -> EvalStepResult

eval1 (Application t1 t2) =
  case (t1, t2) of

  (Abstraction t1', Abstraction _) -> 
    Continue $ shift (-1) 0 $ substitute (makeReplacement 0 $ shift 1 0 t2) t1'

  (_, Abstraction _) -> andThen (\t1' -> Continue $ Application t1' t2) (eval1 t1)

  (Abstraction _, _) -> andThen (Continue . Application t1) (eval1 t2)

  _ -> Stop $ Application t1 t2

eval1 t = Stop t


-- TODO pull this into a better abstraction
andThen :: (Term -> EvalStepResult) ->  EvalStepResult -> EvalStepResult
andThen _ (Error err) = Error err
andThen f (Stop term) = f term
andThen f (Continue term) = f term

