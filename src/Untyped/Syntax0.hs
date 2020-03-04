module Untyped.Syntax0 (Term0(..), Assignment0(..)) where

import Untyped.Parser (ParseTerm(..), ParseAssignment(..))

-- Raw Terms with no De Bruijn Indices

data Term0 =
  Abstraction0 String Term0
  | Application0 Term0 Term0
  | Variable0 String

data Assignment0 =
  Assignment0 String Term0

instance ParseTerm Term0 where
  makeAbstraction var body _ = Abstraction0 var body
  makeApplication t1 t2 _ = Application0 t1 t2
  makeVariable name _ = Variable0 name

instance ParseAssignment Assignment0 where
  -- makeAssignment :: String -> Term0 -> ParseData -> Assignment0
  makeAssignment name term _ = Assignment0 name term
