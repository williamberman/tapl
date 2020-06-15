module Common.Parser
  ( ParseData(..)
  , Position(..)
  , addParseData
  , withParseData
  , eol
  ) where

import qualified Control.Monad                 (liftM, void)
import qualified Text.Parsec.Prim              (ParsecT)
import           Text.ParserCombinators.Parsec (GenParser, SourcePos, char,
                                                getParserState, option,
                                                sourceColumn, sourceLine,
                                                statePos, (<|>))

data ParseData = ParseData
    { start :: Position
    , end   :: Position
    }
    deriving (Show)

data Position = Position
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
eol = Control.Monad.void $ char '\n' <|> (char '\r' >> option '\n' (char '\n'))
