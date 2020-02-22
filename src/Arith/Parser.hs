module Arith.Parser (parseLine, ParseTerm(..), ParseData) where

import Text.ParserCombinators.Parsec

data ParseData = ParseData {
  row :: Int
  , col :: Int
  , text :: String
}

dummyParseData = ParseData { row = 0, col = 0, text = "" }

class ParseTerm a where
  makeTrue :: ParseData -> a
  makeFalse :: ParseData -> a
  makeIf :: ParseData -> a -> a -> a -> a
  makeZero :: ParseData -> a
  makeSucc :: ParseData -> a -> a
  makePred :: ParseData -> a -> a
  makeIsZero :: ParseData -> a -> a
  intToTerm :: ParseData -> Int -> a

parseLine :: ParseTerm a => String -> Either ParseError a
parseLine = parse line "(unknown)"

-- TODO should be able to use either Term or AugmentedTerm
-- which means that the return type is parameterized
line :: ParseTerm a => GenParser Char st a
line = do
  term' <- term
  string ";"
  optional $ string "\n"
  return term'

term :: ParseTerm a => GenParser Char st a
term = do
  optional spaces
  term' <- try true
    <|> try false
    <|> try num
    <|> try succ'
    <|> try pred'
    <|> try iszero
    <|> try if'
  optional spaces
  return term'

true :: ParseTerm a => GenParser Char st a
true = string "true" >> return (makeTrue dummyParseData)

false :: ParseTerm a => GenParser Char st a
false = string "false" >> return (makeFalse dummyParseData)

num :: ParseTerm a => GenParser Char st a
num = intToTerm dummyParseData . read <$> many1 digit

succ' :: ParseTerm a => GenParser Char st a
succ' = makeSucc dummyParseData <$> fcall "succ"

pred' :: ParseTerm a => GenParser Char st a
pred' = makePred dummyParseData <$> fcall "pred"

iszero :: ParseTerm a => GenParser Char st a
iszero = makeIsZero dummyParseData <$> fcall "iszero"

if' :: ParseTerm a => GenParser Char st a
if' = do
  string "if"
  spaces
  predicate <- term
  string "then"
  spaces
  consequent <- term
  string "else"
  spaces
  alternative <- term
  spaces
  return $ makeIf dummyParseData predicate consequent alternative

fcall fName = do
  string fName
  string "("
  term' <- term
  string ")"
  return term'
