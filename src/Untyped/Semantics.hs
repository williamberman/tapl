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

eval1' Application{t1=t1', t2=t2'} =
  case (t1', t2') of
    (Abstraction{term=t1''}, Abstraction{}) ->
      Continue $ shift (-1) 0 $ substitute (makeReplacement 0 $ shift 1 0 t2') t1''
    (_, Abstraction{}) ->
      andThen (\t1'' -> Continue $ Application{t1=t1'', t2=t2', parseData=Nothing}) (eval1 t1')
    (Abstraction{}, _) ->
      andThen (\t2'' -> Continue $ Application{t1=t1', t2=t2'', parseData=Nothing}) (eval1 t2')
    (_, _) ->
      andThen (\t1'' -> Continue $ Application{t1=t1'', t2=t2', parseData=Nothing}) (eval1 t1')

eval1' t = Stop t
