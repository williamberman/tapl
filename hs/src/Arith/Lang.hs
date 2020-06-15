module Arith.Lang (repl) where

import           Arith.Parser    (parseLine)
import qualified Arith.Semantics as Semantics (eval)
import           Arith.Syntax    (Term)
import           Lang.Lang       (InternalLang, makeInternalLang, makeLang)

repl = makeLang irepl initialState

irepl :: InternalLang Term ()
irepl = makeInternalLang parseLine eval Arith.Lang.print

eval _ term =
  case Semantics.eval term of
    Left err  -> Left err
    Right res -> Right (res, ())

initialState = ()

print :: Term -> String
print = show
