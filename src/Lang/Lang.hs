module Lang.Lang (makeInternalLang, makeLang, Lang, InternalLang, env, readEvalPrint) where

import Data.List(intercalate)

type Reader internalRep = String -> [Either String internalRep]
type Evaluator internalRep state = state -> internalRep -> Either String (state, String)

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

makeReader :: Show err => (String -> [Either err internalRep]) -> Reader internalRep
makeReader parser input =
  map convertOutput $ parser input
  where
    convertOutput output =
      case output of
        Left err -> Left $ show err
        Right irep -> Right irep

makeEvaluator :: (Show err, Show out) => (state -> irep -> Either err (out, state)) -> Evaluator irep state
makeEvaluator ievaluator state irep =
  case ievaluator state irep of
    Left err -> Left $ show err
    Right (out, state) -> Right (state, show out)

data Lang = Lang
  { readEvalPrint :: String -> Either String (Lang, String)
  , env :: String
  }

makeLang :: Show s => InternalLang i s -> s -> Lang
makeLang irepl state = Lang
  { readEvalPrint = makeReadEvalPrint irepl state
  , env = show state
  }

makeReadEvalPrint :: Show s => InternalLang i s -> s -> String -> Either String (Lang, String)
makeReadEvalPrint irepl state input =
  case read' (Lang.Lang.read irepl) input of
    Left errors -> Left $ intercalate "\n" errors
    Right parsedResults ->
      case eval' (eval irepl) state parsedResults of
        Left err -> Left err
        Right (state', out) -> Right (makeLang irepl state', out)

read' :: Reader i -> String -> Either [String] [i]
read' reader input =
  foldl reducer (Right []) $ reader input
  where
    reducer :: Either [String] [i] -> Either String i -> Either [String] [i]
    reducer (Right parseResults) (Right parseResult) = Right $ parseResults ++ [parseResult]
    reducer (Right parseResults) (Left error) = Left [error]
    reducer (Left errors) (Right parseResult) = Left errors
    reducer (Left errors) (Left error) = Left $ errors ++ [error]

eval' :: Evaluator i s -> s -> [i] -> Either String (s, String)
eval' evaluator state = foldl reducer $ Right (state, "")
  where
    reducer (Right (state', _)) parsed = evaluator state' parsed
    reducer (Left err) _ = Left err
