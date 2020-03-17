module Lang.File (Lang.File.readFile) where

import Lang.Lang(Lang, readEvalPrint)

readFile :: Lang -> String -> Either String (Lang, String)
readFile = readEvalPrint
