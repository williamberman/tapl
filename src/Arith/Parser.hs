module Arith.Parser (parseLine) where

import Text.ParserCombinators.Parsec
import Arith.Syntax(Term(..), intToTerm)

parseLine :: String -> Either ParseError Term
parseLine = parse line "(unknown)"

-- TODO should be able to use either Term or AugmentedTerm
-- which means that the return type is parameterized
line :: GenParser Char st Term
line = do
  term' <- term
  string ";"
  optional $ string "\n"
  return term'

term :: GenParser Char st Term
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

true :: GenParser Char st Term
true = string "true" >> return TTrue

false :: GenParser Char st Term
false = string "false" >> return TFalse

num :: GenParser Char st Term
num = intToTerm . read <$> many1 digit

succ' :: GenParser Char st Term
succ' = Succ <$> fcall "succ"

pred' :: GenParser Char st Term
pred' = Pred <$> fcall "pred"

iszero :: GenParser Char st Term
iszero = IsZero <$> fcall "iszero"

if' :: GenParser Char st Term
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
  return $ If predicate consequent alternative

fcall :: String -> GenParser Char st Term
fcall fName = do
  string fName
  string "("
  term' <- term
  string ")"
  return term'
