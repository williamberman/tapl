module Arith.Lang (repl) where

import Lang.Lang(makeInternalLang, InternalLang, makeLang)
import Arith.Parser(parseLine)
import Arith.Syntax(Term)
import qualified Arith.Semantics as Semantics(eval)

repl = makeLang irepl initialState

irepl :: InternalLang Term ()
irepl = makeInternalLang parseLine eval Arith.Lang.print

eval _ term =
  case Semantics.eval term of
    Left err -> Left err
    Right res -> Right (res, ())

initialState = ()

print :: Term -> String
print = show
