module Arith.Lang (lang) where

import Lang(makeLang)
import Arith.Parser(parseLine)
import Arith.Semantics(eval)

lang = makeLang parseLine eval
