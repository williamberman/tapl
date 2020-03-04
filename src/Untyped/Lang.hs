module Untyped.Lang where

import Lang(makeLang)
import Untyped.Parser(parseLine)
import Untyped.Syntax(applyIndices)
import Untyped.Semantics(eval)

lang = makeLang parseLine (eval . applyIndices)

foo = eval . applyIndices
