module Arith.Lang (repl) where

import REPL.Lang(makeInternalREPL, InternalREPL, makeREPL)
import Arith.Parser(parseLine)
import Arith.Syntax(Term)
import qualified Arith.Semantics as Semantics(eval)

repl = makeREPL irepl initialState printState

irepl :: InternalREPL Term ()
irepl = makeInternalREPL parseLine eval Arith.Lang.print

eval _ term =
  case Semantics.eval term of
    Left err -> Left err
    Right res -> Right (res, ())

initialState = ()

print :: Term -> String
print = show

printState :: () -> String
printState = show
