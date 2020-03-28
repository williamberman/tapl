module Untyped.Parser
  ( parseInput
  ) where

import           Common.Parser                 (ParseData (..), Position (..),
                                                addParseData, eol,
                                                withParseData)
import           Data.List                     (intercalate)
import           Text.ParserCombinators.Parsec
import           Untyped.Syntax                (Assignment (..), Statement (..),
                                                Term (..), Variable (..))

parseInput :: String -> [Either ParseError Statement]
parseInput input =
  if null input
    then []
    else case parseStatement input of
           Left err -> Left err : parseInput (findNextStartPoint input)
           Right (result, parseData) -> Right result : parseInput (removeUpTo parseData input)

findNextStartPoint :: String -> String
findNextStartPoint input = intercalate "\n" $ drop 1 $ lines input

removeUpTo :: ParseData -> String -> String
removeUpTo ParseData {end = Position {row = row', col = col'}} input =
  drop (col' - 1) $ intercalate "\n" $ drop (row' - 1) $ lines input

parseStatement :: String -> Either ParseError (Statement, ParseData)
parseStatement input = parse (withParseData topLevel) input input

topLevel :: GenParser Char st Statement
topLevel = try statement <|> nop

statement :: GenParser Char st Statement
statement = do
  optional spaces
  parsed <- try statementAssignment <|> statementTerm
  string ";"
  optional spaces
  return parsed

nop = try emptyLine <|> try comment

emptyLine = do
  emptyLine'
  return Nop

emptyLine' = try eol <|> try eof <|> do { space; emptyLine' }

comment = do
  string "--"
  comment'
  return Nop

comment' = try eol <|> try eof <|> do { anyChar; comment' }

statementAssignment :: GenParser Char st Statement
statementAssignment = StatementAssignment <$> assignment

statementTerm :: GenParser Char st Statement
statementTerm = StatementTerm <$> parseTerm

assignment :: GenParser Char st Assignment
assignment =
  addParseData $ do
    varIdent <- variableIdentifier
    optional spaces
    char '='
    optional spaces
    Assignment varIdent <$> parseTerm

parseTerm :: GenParser Char st Term
parseTerm = do
  optional spaces
  term' <- try abstraction <|> try application <|> atomicTerm
  optional spaces
  return term'

atomicTerm :: GenParser Char st Term
atomicTerm = try parenTerm <|> variable

parenTerm :: GenParser Char st Term
parenTerm = do
  char '('
  term' <- parseTerm
  char ')'
  return term'

justTerm :: GenParser Char st Term
justTerm = try abstraction <|> try variable <|> try application

abstraction :: GenParser Char st Term
abstraction =
  addMaybeParseData $ do
    string "lambda"
    spaces
    varIdent <- try variableIdentifier <|> string "_"
    optional spaces
    char '.'
    parsedTerm <- parseTerm
    return $ \pd -> Abstraction {parseData = pd, name = varIdent, term = parsedTerm}

variable :: GenParser Char st Term
variable =
  addMaybeParseData $ do
    ident <- variableIdentifier
    return $ \pd -> Variable {parseData = pd, var = NamedVariable {variableName = ident}}

application :: GenParser Char st Term
application =
  atomicTerm `chainl1`
  addMaybeParseData
    (do spaces
        return $ \pd t1' t2' -> Application {t1 = t1', t2 = t2', parseData = pd})

variableIdentifier :: GenParser Char st String
variableIdentifier = many1 alphaNum

addMaybeParseData :: GenParser Char st (Maybe ParseData -> a) -> GenParser Char st a
addMaybeParseData parser = do
  (result, parseData) <- withParseData parser
  return $ result $ Just parseData
