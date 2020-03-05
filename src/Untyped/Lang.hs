module Untyped.Lang(irepl, initialState) where

import Untyped.Parser(parseStatement, ParseStatement(..), ParseAssignment(..))
import Untyped.Syntax(applyIndices, Term(..))
import qualified Untyped.Semantics as Semantics(eval)
import Untyped.Syntax0(parseTermToTerm0, parseStatementToStatement0, Statement0(..), Assignment0(..))
import Untyped.State
import REPL(makeInternalREPL, InternalREPL)
import Text.ParserCombinators.Parsec(ParseError)

import Common.Semantics

irepl :: InternalREPL Statement0 State
irepl = makeInternalREPL parseStatement' eval Untyped.Lang.print

parseStatement' :: String -> Either ParseError Statement0
parseStatement' inp =
  case parseStatement inp of
    Left err -> Left err
    Right parsed -> Right $ parseStatementToStatement0 parsed

eval :: State -> Statement0 -> Either (EvalError Term) (Term, State)

eval state (StatementTerm term) =
  case Semantics.eval term' of
    Left error -> Left error
    Right term'' -> Right (term'', state)
  where
    (term', state) = applyIndices term

eval state (StatementAssignment (Assignment0 _ term)) =
  Left EvalError{ t = term', message = "Assignment not yet implemented" }
  where
    (term', _) = applyIndices term

print :: Statement0 -> String
print = show
