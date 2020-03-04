module Untyped.Parser (parseStatement, ParseTerm(..), ParseAssignment(..), ParseStatement(..)) where

import Text.ParserCombinators.Parsec
import Common.Parser

data ParseStatement a b =
  ParsedAssignment a
  | ParsedTerm b

class ParseAssignment a where
  makeAssignment :: String -> b -> ParseData -> a

class ParseTerm a where
  makeAbstraction :: String -> a -> ParseData -> a
  makeVariable :: String -> ParseData -> a
  makeApplication :: a -> a -> ParseData -> a

parseStatement :: ParseTerm a => ParseTerm b => ParseAssignment a => String -> Either ParseError (ParseStatement a b)
parseStatement = parse statement "(unknown)"

statement :: ParseTerm a => ParseTerm b => ParseAssignment a => GenParser Char st (ParseStatement a b)
statement = do
  optional spaces
  parsed <- try statementAssignment
    <|> try statementTerm
  string ";"
  optional spaces
  return parsed

statementAssignment :: ParseAssignment a => ParseTerm a => GenParser Char st (ParseStatement a b)
statementAssignment = ParsedAssignment <$> assignment

statementTerm ::  ParseTerm b => GenParser Char st (ParseStatement a b)
statementTerm = ParsedTerm <$> term

assignment :: ParseAssignment a => ParseTerm a => GenParser Char st a
assignment =
  addParseData $ do
    varIdent <- variableIdentifier
    optional spaces
    char '='
    optional spaces
    makeAssignment varIdent <$> term

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
    optional spaces
    t1 <- term
    optional spaces
    char ')'
    optional spaces
    char '('
    optional spaces
    t2 <- term
    optional spaces
    char ')'
    return $ makeApplication t1 t2

variableIdentifier :: GenParser Char st String
variableIdentifier = many1 alphaNum
