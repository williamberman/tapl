module Untyped.Lang(repl) where

import Untyped.Parser(parseStatement, ParseStatement(..), ParseAssignment(..))
import Untyped.Syntax(applyIndices, Term(..))
import qualified Untyped.Semantics as Semantics(eval)
import Untyped.Syntax0(parseTermToTerm0, parseStatementToStatement0, Statement0(..), Assignment0(..), Term0)
import Untyped.State(State, initialState, makeState, setForm, printState)
import qualified Untyped.State(printState)
import REPL.Lang(makeInternalREPL, InternalREPL, makeREPL)
import Text.ParserCombinators.Parsec(ParseError)

import Common.Semantics

repl = makeREPL irepl initialState printState

irepl :: InternalREPL Statement0 (State Term0)
irepl = makeInternalREPL parseStatement' eval Untyped.Lang.print

parseStatement' :: String -> Either ParseError Statement0
parseStatement' inp =
  case parseStatement inp of
    Left err -> Left err
    Right parsed -> Right $ parseStatementToStatement0 parsed

eval :: State Term0 -> Statement0 -> Either (EvalError Term) (Term, State Term0)

eval state (StatementTerm term) =
  case Semantics.eval term' of
    Left error -> Left error
    Right term'' -> Right (term'', makeState env state)
  where
    (term', env) = applyIndices term

eval state (StatementAssignment (Assignment0 name term)) =
  case Semantics.eval term' of
    Left error -> Left error
    Right term'' -> Right (term'', setForm name term $ makeState env state)
  where
    (term', env) = applyIndices term

print :: Statement0 -> String
print = show
