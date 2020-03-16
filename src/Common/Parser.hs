module Common.Parser
  ( ParseData(..)
  , Position(..)
  , addParseData
  , withParseData
  , eol
  ) where

import qualified Control.Monad
import qualified Text.Parsec.Prim
import           Text.ParserCombinators.Parsec

data ParseData =
  ParseData
    { start :: Position
    , end   :: Position
    }
  deriving (Show)

data Position =
  Position
    { row :: Int
    , col :: Int
    }
  deriving (Show)

addParseData :: GenParser Char st (ParseData -> a) -> GenParser Char st a
addParseData parser = do
  (result, parseData) <- withParseData parser
  return $ result parseData

withParseData :: GenParser Char st a -> GenParser Char st (a, ParseData)
withParseData parser = do
  startPos <- sourcePos
  result <- parser
  endPos <- sourcePos
  let parseData =
        ParseData
          { start = Position {row = sourceLine startPos, col = sourceColumn startPos}
          , end = Position {row = sourceLine endPos, col = sourceColumn endPos}
          }
  return (result, parseData)

sourcePos :: Monad m => Text.Parsec.Prim.ParsecT s u m SourcePos
sourcePos = statePos `Control.Monad.liftM` getParserState

eol :: GenParser Char state ()
eol = (char '\n' <|> (char '\r' >> option '\n' (char '\n'))) >> return ()
