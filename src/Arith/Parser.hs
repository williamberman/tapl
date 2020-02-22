module Arith.Parser (parseLine, ParseTerm(..)) where

import Text.ParserCombinators.Parsec

class ParseTerm a where
  makeTrue :: a
  makeFalse :: a
  makeIf :: a -> a -> a -> a
  makeZero :: a
  makeSucc :: a -> a
  makePred :: a -> a
  makeIsZero :: a -> a
  intToTerm :: Int -> a

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
true = string "true" >> return makeTrue

false :: ParseTerm a => GenParser Char st a
false = string "false" >> return makeFalse

num :: ParseTerm a => GenParser Char st a
num = intToTerm . read <$> many1 digit

succ' :: ParseTerm a => GenParser Char st a
succ' = makeSucc <$> fcall "succ"

pred' :: ParseTerm a => GenParser Char st a
pred' = makePred <$> fcall "pred"

iszero :: ParseTerm a => GenParser Char st a
iszero = makeIsZero <$> fcall "iszero"

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
  return $ makeIf predicate consequent alternative

fcall fName = do
  string fName
  string "("
  term' <- term
  string ")"
  return term'
