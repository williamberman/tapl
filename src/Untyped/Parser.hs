module Untyped.Parser (parseLine, ParseTerm(..)) where

import Text.ParserCombinators.Parsec
import Common.Parser

class ParseTerm a where
  makeAbstraction :: String -> a -> ParseData -> a
  makeVariable :: String -> ParseData -> a
  makeApplication :: a -> a -> ParseData -> a

parseLine :: ParseTerm a => String -> Either ParseError a
parseLine = parse line "(unknown)"

line :: ParseTerm a => GenParser Char st a
line = do
  term' <- term
  string ";"
  optional eol
  return term'

term :: ParseTerm a => GenParser Char st a
term = do
  optional spaces
  term' <- try abstraction
    <|> try variable
    <|> application
  optional spaces
  return term'

abstraction :: ParseTerm a => GenParser Char st a
abstraction =
  addParseData $ do
    string "lambda"
    spaces
    varIdent <- variableIdentifier
    string "."
    makeAbstraction varIdent <$> term


variable :: ParseTerm a => GenParser Char st a
variable = addParseData $ makeVariable <$> variableIdentifier

application :: ParseTerm a => GenParser Char st a
application =
  addParseData $ do
    char '('
    spaces
    t1 <- term
    spaces
    char ')'
    spaces
    char '('
    spaces
    t2 <- term
    spaces
    char ')'
    return $ makeApplication t1 t2

variableIdentifier :: GenParser Char st String
variableIdentifier = many1 alphaNum
