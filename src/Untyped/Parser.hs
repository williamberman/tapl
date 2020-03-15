module Untyped.Parser (parseStatement, ParseStatement(..), ParseTerm(..), ParseAssignment(..)) where

import Text.ParserCombinators.Parsec
import Common.Parser

-- TODO, I'd like to abstract the behavior for these concrete data definitions into typeclasses

data ParseStatement =
  ParsedTerm ParseTerm
  | ParsedAssignment ParseAssignment
  | ParsedNop
  deriving Show

data ParseTerm =
  ParseAbstraction String ParseTerm ParseData
  -- TODO
  -- | ParseApplication ParseTerm ParseTerm ParseData
  | ParseApplication ParseTerm ParseTerm
  | ParseVariable String ParseData
  deriving Show

data ParseAssignment =
  ParseAssignment String ParseTerm ParseData
  deriving Show

parseStatement :: String -> Either ParseError ParseStatement
parseStatement input = parse topLevel input input

topLevel :: GenParser Char st ParseStatement
topLevel = try statement <|> nop

statement :: GenParser Char st ParseStatement
statement = do
  optional spaces
  parsed <- try statementAssignment <|> statementTerm
  string ";"
  optional spaces
  eof
  return parsed
  
nop = try emptyLine <|> try comment

emptyLine = do { optional spaces; optional eol; eof; return ParsedNop }

comment = do { string "--"; many anyChar ; optional eol ; eof; return ParsedNop }

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
  term' <- try abstraction <|> try application <|> atomicTerm
  optional spaces
  return term'

atomicTerm :: GenParser Char st ParseTerm
atomicTerm = try parenTerm <|> variable

parenTerm :: GenParser Char st ParseTerm
parenTerm = do
  char '('
  term' <- term
  char ')'
  return term'

justTerm :: GenParser Char st ParseTerm
justTerm = try abstraction <|> try variable <|> try application

abstraction :: GenParser Char st ParseTerm
abstraction =
  addParseData $ do
    string "lambda"
    spaces
    varIdent <- try variableIdentifier <|> string "_"
    optional spaces
    char '.'
    ParseAbstraction varIdent <$> term


variable :: GenParser Char st ParseTerm
variable = addParseData $ ParseVariable <$> variableIdentifier

application :: GenParser Char st ParseTerm
application = atomicTerm `chainl1` do { spaces; return ParseApplication }

variableIdentifier :: GenParser Char st String
variableIdentifier = many1 alphaNum
