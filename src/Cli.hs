module Cli (opts, Args(..)) where

import           Lib                 (LangSelector (..))

import           Data.Semigroup      ((<>))
import           Options.Applicative

data Args = Args
    { lang :: LangSelector
    , file :: Maybe String
    }
    deriving Show

opts = info (sample <**> helper)
     ( fullDesc
     <> progDesc "A collection of programming languages from Types and Programming Languages" )

sample :: Parser Args
sample = Args
  <$> langParser
  -- TODO should be positional
  <*> optional (strOption $
      metavar "FILE"
      <> long "file"
      <> short 'f'
      <> help "Optional file to read from. If left out, boots into REPL." )


langParser :: Parser LangSelector
langParser = option auto
            ( long "lang"
           <> short 'l'
           <> metavar "LANG"
           <> help "Language to execute" )
