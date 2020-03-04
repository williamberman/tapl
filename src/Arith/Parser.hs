module Arith.Parser (parseLine, ParseTerm(..), ParseData) where

import Text.ParserCombinators.Parsec
import Common.Parser

class ParseTerm a where
  makeTrue :: ParseData -> a
  makeFalse :: ParseData -> a
  makeIf :: a -> a -> a -> ParseData ->  a
  makeZero :: ParseData -> a
  makeSucc :: a -> ParseData -> a
  makePred :: a -> ParseData -> a
  makeIsZero :: a -> ParseData -> a
  intToTerm :: Int -> ParseData -> a

parseLine :: ParseTerm a => String -> Either ParseError a
parseLine = parse line "(unknown)"

line :: ParseTerm a => GenParser Char st a
line = do
  term' <- term
  eol
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
true = addParseData $ string "true" >> return makeTrue

false :: ParseTerm a => GenParser Char st a
false = addParseData $ string "false" >> return makeFalse

num :: ParseTerm a => GenParser Char st a
num = addParseData $ intToTerm . read <$> many1 digit

succ' :: ParseTerm a => GenParser Char st a
succ' = addParseData $ makeSucc <$> fcall "succ"

pred' :: ParseTerm a => GenParser Char st a
pred' = addParseData $ makePred <$> fcall "pred"

iszero :: ParseTerm a => GenParser Char st a
iszero = addParseData $ makeIsZero <$> fcall "iszero"

if' :: ParseTerm a => GenParser Char st a
if' = addParseData $ do
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
  return $ makeIf predicate consequent alternative

fcall fName = do
  string fName
  string "("
  term' <- term
  string ")"
  return term'
