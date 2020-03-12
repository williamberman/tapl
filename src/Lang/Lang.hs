module Lang.Lang (makeInternalLang, makeLang, Lang, InternalLang, env, readEvalPrint) where

type Reader internalRep = String -> Either String internalRep
type Evaluator internalRep state = state -> internalRep -> Either String (String, state)

data InternalLang internalRep state = InternalLang
  { read :: Reader internalRep
  , eval :: Evaluator internalRep state
  , print :: internalRep -> String
  }

makeInternalLang parser ievaluator printer = InternalLang
  { Lang.Lang.read = makeReader parser
  , eval = makeEvaluator ievaluator
  , Lang.Lang.print = printer
  }

makeReader :: Show err => (String -> Either err internalRep) -> Reader internalRep
makeReader parser input =
  case parser input of
    Left err -> Left $ show err
    Right irep -> Right irep

makeEvaluator :: (Show err, Show out) => (state -> irep -> Either err (out, state)) -> Evaluator irep state
makeEvaluator ievaluator state irep =
  case ievaluator state irep of
    Left err -> Left $ show err
    Right (out, state) -> Right (show out, state)

data Lang = Lang
  { readEvalPrint :: String -> Either String (String, Lang)
  , env :: String
  }

makeLang :: Show s => InternalLang i s -> s -> Lang
makeLang irepl state = Lang
  { readEvalPrint = makeReadEvalPrint irepl state
  , env = show state
  }

makeReadEvalPrint :: Show s => InternalLang i s -> s -> String -> Either String (String, Lang)
makeReadEvalPrint irepl state input =
  case Lang.Lang.read irepl input of
    Left err -> Left err
    Right iRep ->
      case eval irepl state iRep of
        Left err -> Left err
        Right (out, state') -> Right (out, makeLang irepl state')
