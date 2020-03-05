module Untyped.Lang where

import Lang(makeLang)
import Untyped.Parser(parseStatement, ParseStatement(..), ParseAssignment(..))
import Untyped.Syntax(applyIndices, Term(..))
import qualified Untyped.Semantics as Semantics(eval)
import Untyped.Syntax0(parseTermToTerm0)

import Common.Semantics

lang = makeLang parseStatement eval

eval :: ParseStatement -> Either (EvalError Term) Term
eval (ParsedTerm term) = 
  Semantics.eval term'
  where
    (term', _) = applyIndices $ parseTermToTerm0 term
eval (ParsedAssignment (ParseAssignment _ term _)) = 
  Left EvalError{ t = term', message = "Assignment not yet implemented" }
  where
    (term', _) = applyIndices $ parseTermToTerm0 term
