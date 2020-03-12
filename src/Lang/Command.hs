module Lang.Command (parseCommand, Command(..)) where

import Text.ParserCombinators.Parsec

data Command =
  Quit
  | Env
  | LoadFile String

parseCommand :: String -> Either ParseError Command
parseCommand input = parse command input input

command :: GenParser Char st Command
command = do
  char ':'
  command' <- try quit <|> try env <|> loadFile
  optional spaces
  eof
  return command'

quit :: GenParser Char st Command
quit = do { string "quit" ; return Quit }

env :: GenParser Char st Command
env = do { string "env" ; return Env }

loadFile :: GenParser Char st Command
loadFile = do
  string "load"
  spaces
  filename <- many1 anyChar
  return $ LoadFile filename
