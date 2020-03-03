module Lang (makeLang, readEval, Lang) where

type Reader term = String -> Either String term
type Evaluator term = term -> Either String String

data Lang term = Lang
  { read :: Reader term
    , eval :: Evaluator term
  }

makeLang parser termEvaluator = Lang
  { Lang.read = makeReader parser
  , Lang.eval = makeEvaluator termEvaluator
  }

readEval :: Lang term -> String -> Either String String
readEval (Lang {Lang.read = read, eval = eval}) input =
  case read input of
    Left err -> Left err
    Right parsed -> eval parsed

makeReader :: Show err => (String -> Either err term) -> Reader term
makeReader parser input =
  case parser input of
    Left err -> Left $ show err
    Right term -> Right term

makeEvaluator :: Show err => Show out => (term -> Either err out) -> Evaluator term
makeEvaluator termEvaluator term =
  case termEvaluator term of
    Left err -> Left $ show err
    Right out -> Right $ show out
