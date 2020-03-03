module Arith.Lang (lang) where

import Lang(makeLang, Lang)
import Arith.Parser(parseLine)
import Arith.Semantics(eval)

lang = makeLang parseLine eval
