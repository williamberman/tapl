module Common.Parser (ParseData(..), Position(..), addParseData, eol) where

import qualified Text.Parsec.Prim
import qualified Control.Monad
import Text.ParserCombinators.Parsec

data ParseData = ParseData {
  start :: Position,
  end :: Position
}

data Position = Position {
  row :: Int
  , col :: Int
}

addParseData :: GenParser Char st (ParseData -> a) -> GenParser Char st a
addParseData parser = do
  startPos <- sourcePos
  result <- parser
  endPos <- sourcePos
  return $ result ParseData {
    start = Position { row = sourceLine startPos, col = sourceColumn startPos }
    , end = Position { row = sourceLine endPos, col = sourceColumn endPos }
  }

sourcePos :: Monad m => Text.Parsec.Prim.ParsecT s u m SourcePos
sourcePos = statePos `Control.Monad.liftM` getParserState

eol :: GenParser Char state ()
eol = (char '\n' <|> (char '\r' >> option '\n' (char '\n'))) >> return ()
