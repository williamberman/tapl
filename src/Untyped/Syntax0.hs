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
  
instance Show Term0 where
  show (Abstraction0 name term) = "lambda " <> name <> ". " <> show term
  show (Application0 (Application0 t11 t12) t2) = "(" <> show (Application0 t11 t12) <> ") " <> show t2
  show (Application0 t1 t2) = show t1 <> " " <> show t2
  show (Variable0 name) = name

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
