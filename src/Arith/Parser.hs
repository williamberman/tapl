module Arith.Parser (parseLine) where

import Text.ParserCombinators.Parsec
import Arith.Termable(Termable(..))

parseLine :: Termable a => String -> Either ParseError a
parseLine = parse line "(unknown)"

-- TODO should be able to use either Term or AugmentedTerm
-- which means that the return type is parameterized
line :: Termable a => GenParser Char st a
line = do
  term' <- term
  string ";"
  optional $ string "\n"
  return term'

term :: Termable a => GenParser Char st a
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

true :: Termable a => GenParser Char st a
true = string "true" >> return makeTrue

false :: Termable a => GenParser Char st a
false = string "false" >> return makeFalse

num :: Termable a => GenParser Char st a
num = intToTerm . read <$> many1 digit

succ' :: Termable a => GenParser Char st a
succ' = makeSucc <$> fcall "succ"

pred' :: Termable a => GenParser Char st a
pred' = makePred <$> fcall "pred"

iszero :: Termable a => GenParser Char st a
iszero = makeIsZero <$> fcall "iszero"

if' :: Termable a => GenParser Char st a
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
