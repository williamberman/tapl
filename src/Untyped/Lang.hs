module Untyped.Lang(repl) where

import Untyped.Parser(parseInput)
import Untyped.Syntax(applyIndices, Term(..), Statement(..), Assignment(..))
import qualified Untyped.Semantics as Semantics(eval)
import Untyped.State(State, initialState, makeState, setForm)
import Lang.Lang(makeInternalLang, InternalLang, makeLang)
import Text.ParserCombinators.Parsec(ParseError)

import Common.Semantics

repl = makeLang irepl initialState

irepl :: InternalLang Statement (State Term)
irepl = makeInternalLang parseInput eval Untyped.Lang.print

newtype EvalOut = EvalOut (Maybe Term)

instance Show EvalOut where
  show (EvalOut Nothing) = ""
  show (EvalOut (Just t)) = show t

eval :: State Term -> Statement -> Either (EvalError Term) (EvalOut, State Term)
eval state (StatementTerm term) =
  case Semantics.eval term' of
    Left error -> Left error
    Right term'' -> Right (EvalOut $ Just term'', makeState env state)
  where
    (term', env) = applyIndices term
eval state (StatementAssignment (Assignment name term _)) =
  case Semantics.eval term' of
    Left error -> Left error
    Right term'' -> Right (EvalOut $ Just term'', setForm name term $ makeState env state)
  where
    (term', env) = applyIndices term

eval state Nop = Right (EvalOut Nothing, state)

print :: Statement -> String
print = show
