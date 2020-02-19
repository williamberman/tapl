module Arith.Parser (parseLine) where

import Text.ParserCombinators.Parsec
import Arith.Syntax(Term(..), intToTerm)

parseLine :: String -> Either ParseError Term
parseLine = parse line "(unknown)"

-- TODO this should likely use the augmented
-- version of the Terms
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

-- TODO don't understand why need "read <$>"
num :: GenParser Char st Term
num = read <$> many1 digit >>= return . intToTerm

succ' :: GenParser Char st Term
succ' = fcall "succ" >>= return . Succ

pred' :: GenParser Char st Term
pred' = fcall "pred" >>= return . Pred

iszero :: GenParser Char st Term
iszero = fcall "iszero" >>= return . Pred

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
  return $ If predicate consequent alternative

fcall :: String -> GenParser Char st Term
fcall fName = do
  string fName
  string "("
  term' <- term
  string ")"
  return term'
