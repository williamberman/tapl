module Untyped.Syntax0 (Statement0(..), Assignment0(..), Term0(..), parseTermToTerm0, parseStatementToStatement0) where

import Untyped.Parser (ParseTerm(..), ParseAssignment(..), ParseStatement(..))

-- Raw Terms with no De Bruijn Indices

data Statement0 =
  StatementTerm Term0
  | StatementAssignment Assignment0
  deriving Show

data Term0 =
  Abstraction0 String Term0
  | Application0 Term0 Term0
  | Variable0 String
  deriving Show

data Assignment0 =
  Assignment0 String Term0
  deriving Show

parseStatementToStatement0 :: ParseStatement -> Statement0
parseStatementToStatement0 (ParsedTerm term) = StatementTerm $ parseTermToTerm0 term
parseStatementToStatement0 (ParsedAssignment assign) = StatementAssignment $ parseAssignmentToAssignment0 assign

parseTermToTerm0 :: ParseTerm -> Term0
parseTermToTerm0 (ParseAbstraction name term _) = Abstraction0 name $ parseTermToTerm0 term
-- TODO
-- parseTermToTerm0 (ParseApplication t1 t2 _) = Application0 (parseTermToTerm0 t1) (parseTermToTerm0 t2)
parseTermToTerm0 (ParseApplication t1 t2) = Application0 (parseTermToTerm0 t1) (parseTermToTerm0 t2)
parseTermToTerm0 (ParseVariable name _) = Variable0 name

parseAssignmentToAssignment0 :: ParseAssignment -> Assignment0
parseAssignmentToAssignment0 (ParseAssignment name term _) = Assignment0 name $ parseTermToTerm0 term
