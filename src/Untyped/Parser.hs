module Untyped.Parser (parseStatement, ParseStatement(..), ParseTerm(..), ParseAssignment(..)) where

import Text.ParserCombinators.Parsec
import Common.Parser

-- TODO, I'd like to abstract the behavior for these concrete data definitions into typeclasses

data ParseStatement =
  ParsedTerm ParseTerm
  | ParsedAssignment ParseAssignment

data ParseTerm =
  ParseAbstraction String ParseTerm ParseData
  | ParseApplication ParseTerm ParseTerm ParseData
  | ParseVariable String ParseData

data ParseAssignment =
  ParseAssignment String ParseTerm ParseData

parseStatement :: String -> Either ParseError ParseStatement
parseStatement = parse statement "(unknown)"

statement :: GenParser Char st ParseStatement
statement = do
  optional spaces
  parsed <- try statementAssignment
    <|> try statementTerm
  string ";"
  optional spaces
  return parsed

statementAssignment :: GenParser Char st ParseStatement
statementAssignment = ParsedAssignment <$> assignment

statementTerm :: GenParser Char st ParseStatement
statementTerm = ParsedTerm <$> term

assignment :: GenParser Char st ParseAssignment
assignment =
  addParseData $ do
    varIdent <- variableIdentifier
    optional spaces
    char '='
    optional spaces
    ParseAssignment varIdent <$> term

term :: GenParser Char st ParseTerm
term = do
  optional spaces
  term' <- try parenTerm
    <|> try justTerm
  optional spaces
  return term'

parenTerm :: GenParser Char st ParseTerm
parenTerm = do
  char '('
  optional spaces
  term' <- justTerm
  optional spaces
  char ')'
  return term'

justTerm :: GenParser Char st ParseTerm
justTerm = try abstraction <|> try variable <|> try application

abstraction :: GenParser Char st ParseTerm
abstraction =
  addParseData $ do
    string "lambda"
    spaces
    varIdent <- variableIdentifier
    string "."
    ParseAbstraction varIdent <$> term


variable :: GenParser Char st ParseTerm
variable = addParseData $ ParseVariable <$> variableIdentifier

application :: GenParser Char st ParseTerm
application =
  addParseData $ do
    t1 <- term
    ParseApplication t1 <$> term

variableIdentifier :: GenParser Char st String
variableIdentifier = many1 alphaNum
