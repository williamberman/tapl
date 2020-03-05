module Arith.Lang (irepl, initialState) where

import REPL(makeInternalREPL, InternalREPL)
import Arith.Parser(parseLine)
import Arith.Syntax(Term)
import qualified Arith.Semantics as Semantics(eval)

irepl :: InternalREPL Term ()
irepl = makeInternalREPL parseLine eval Arith.Lang.print

eval _ term =
  case Semantics.eval term of
    Left err -> Left err
    Right res -> Right (res, ())

initialState = ()

print :: Term -> String
print = show
