module Untyped.Lang(repl) where

import Untyped.Parser(parseStatement, ParseStatement(..), ParseAssignment(..))
import Untyped.Syntax(applyIndices, Term(..))
import qualified Untyped.Semantics as Semantics(eval)
import Untyped.Syntax0(parseTermToTerm0, parseStatementToStatement0, Statement0(..), Assignment0(..), Term0)
import Untyped.State(State, initialState, makeState, setForm)
import Lang.Lang(makeInternalLang, InternalLang, makeLang)
import Text.ParserCombinators.Parsec(ParseError)

import Common.Semantics

repl = makeLang irepl initialState

irepl :: InternalLang Statement0 (State Term0)
irepl = makeInternalLang parseStatement' eval Untyped.Lang.print

parseStatement' :: String -> Either ParseError Statement0
parseStatement' inp =
  case parseStatement inp of
    Left err -> Left err
    Right parsed -> Right $ parseStatementToStatement0 parsed

newtype EvalOut = EvalOut (Maybe Term)

instance Show EvalOut where
  show (EvalOut Nothing) = ""
  show (EvalOut (Just t)) = show t

eval :: State Term0 -> Statement0 -> Either (EvalError Term) (EvalOut, State Term0)

eval state (StatementTerm term) =
  case Semantics.eval term' of
    Left error -> Left error
    Right term'' -> Right (EvalOut $ Just term'', makeState env state)
  where
    (term', env) = applyIndices term

eval state (StatementAssignment (Assignment0 name term)) =
  case Semantics.eval term' of
    Left error -> Left error
    Right term'' -> Right (EvalOut $ Just term'', setForm name term $ makeState env state)
  where
    (term', env) = applyIndices term

eval state StatementNop = Right (EvalOut Nothing, state)

print :: Statement0 -> String
print = show
