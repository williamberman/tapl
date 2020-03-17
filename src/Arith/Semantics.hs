module Arith.Semantics (eval) where

import Arith.Syntax(Term(..), isNumeric)
import Common.Semantics

----- eval -----

data Out =
  OutB Bool
  | OutI Int

instance Show Out where
  show (OutB b) = show b
  show (OutI i) = show i

eval :: Term -> Either (EvalError Term) Out
eval term = case eval1 term of
  Error err -> Left err
  Continue next -> eval next
  Stop term' -> convertToOutput term'

----- eval1 -----

eval1 :: Term -> EvalStepResult Term

-- If
eval1 (If TTrue consequent _) = Continue consequent
eval1 (If TFalse _ alternative) = Continue alternative
eval1 (If term consequent alternative) =
   andThen (\pred' -> Continue $ If pred' consequent alternative) (eval1 term)

-- Succ
eval1 (Succ (Pred term)) =
  if isNumeric term then
    Continue term
  else
    Error $ EvalError { t = term, message = "Cannot apply simplification to non numerical argument" }
eval1 (Succ term) =
  if isNumeric term then
    Stop $ Succ term
  else
    andThen (Continue . Succ) (eval1 term)

-- Pred
eval1 (Pred Zero) = Continue Zero
eval1 (Pred (Succ term)) =
  if isNumeric term then
    Continue term
  else
    Error $ EvalError { t = term, message = "Cannot apply simplification to non numerical argument" }
eval1 (Pred term) =
  if isNumeric term then
    Stop $ Pred term
  else
    andThen (Continue . Pred) (eval1 term)

-- IsZero
eval1 (IsZero Zero) = Continue TTrue
eval1 (IsZero (Succ term)) =
  if isNumeric term then
    andThen (Continue . IsZero . Succ) (eval1 term)
  else
    Error $ EvalError { t = term, message = "Cannot check IsZero of non numerical argument" }
eval1 (IsZero term) = andThen (Continue . IsZero) (eval1 term)

eval1 term = Stop term

------ convertToOutput ------

convertToOutput :: Term -> Either (EvalError Term) Out

convertToOutput TTrue = Right $ OutB True
convertToOutput TFalse = Right $ OutB False
convertToOutput Zero = Right $ OutI 0
convertToOutput (Succ term) = case convertToOutput term of
  Right (OutI n) -> Right $ OutI $ n + 1
  Right (OutB _) -> Left $ EvalError { t = term, message = "Cannot add to Boolean" }
  Left err -> Left err
convertToOutput (Pred term) = case convertToOutput term of
  Right (OutI n) -> Right $ OutI $ if n == 0 then 0 else n - 1
  Right (OutB _) -> Left $ EvalError { t = term, message = "Cannot subtract from Boolean" }
  Left err -> Left err
convertToOutput term =
  Left $ EvalError { t = term
                   , message = "Could not complete evaluation"
                   }
