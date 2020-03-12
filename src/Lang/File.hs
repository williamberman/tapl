module Lang.File (Lang.File.readFile) where

import Lang.Lang(Lang, readEvalPrint)

readFile :: Lang -> String -> Either String (Lang, Maybe String)
-- TODO this should not break the input into lines
readFile lang content = readFileHelper Nothing lang $ lines content

readFileHelper :: Maybe String -> Lang -> [String] -> Either String (Lang, Maybe String)
readFileHelper prev lang [] = Right (lang, prev)
readFileHelper _ lang (line : lines) =
    case readEvalPrint lang line of
      Left err -> Left err
      Right (out, lang') -> readFileHelper (Just out) lang' lines
