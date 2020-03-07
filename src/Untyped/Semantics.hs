module Untyped.Semantics (eval) where

import Untyped.Syntax(Term(..), makeReplacement, substitute, shift)
import Common.Semantics

eval :: Term -> Either (EvalError Term) Term

eval term = case eval1 term of
  Error err -> Left err
  Continue next -> eval next
  Stop term' -> Right term'

eval1 :: Term -> EvalStepResult Term
eval1 = stopIfSame eval1'

-- Repetition in implementation ensures consistent reduction order
eval1' :: Term -> EvalStepResult Term

eval1' (Application (Abstraction t1 _) (Abstraction t2 n2)) =
    Continue $ shift (-1) 0 $ substitute (makeReplacement 0 $ shift 1 0 $ Abstraction t2 n2) t1

eval1' (Application t1 (Abstraction t2 n2)) =
  andThen (\t1' -> Continue $ Application t1' $ Abstraction t2 n2) (eval1 t1)

eval1' (Application (Abstraction t1 n1) t2) =
  andThen (Continue . Application (Abstraction t1 n1)) (eval1 t2)

eval1' (Application t1 t2) =
  andThen (\t1' -> Continue $ Application t1' t2) (eval1 t1)

eval1' t = Stop t
