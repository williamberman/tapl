module REPL (makeInternalREPL, makeREPL, readEvalPrint, REPL, InternalREPL) where

type Reader internalRep = String -> Either String internalRep
type Evaluator internalRep state = state -> internalRep -> Either String (String, state)

data InternalREPL internalRep state = InternalREPL
  { read :: Reader internalRep
  , eval :: Evaluator internalRep state
  , print :: internalRep -> String
  }

makeInternalREPL parser ievaluator printer = InternalREPL
  { REPL.read = makeReader parser
  , eval = makeEvaluator ievaluator
  , REPL.print = printer
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

newtype REPL = REPL
  { readEvalPrint :: String -> Either String (String, REPL)
  }

makeREPL :: InternalREPL i s -> s -> REPL
makeREPL irepl state = REPL
  { readEvalPrint = makeReadEvalPrint irepl state
  }

makeReadEvalPrint :: InternalREPL i s -> s -> String -> Either String (String, REPL)
makeReadEvalPrint irepl state input =
  case REPL.read irepl input of
    Left err -> Left err
    Right iRep ->
      case eval irepl state iRep of
        Left err -> Left err
        Right (out, state') -> Right (out, makeREPL irepl state')
