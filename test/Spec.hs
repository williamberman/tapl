import Test.Tasty
import Test.Tasty.HUnit

import Untyped.Syntax(Term(..))
import Untyped.Semantics(eval)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [untyped]

untyped = testGroup "Untyped lambda calculus"
  [ testCase "Leaves index of bound variable on substitution" $
      eval (Application (Abstraction $ Abstraction $ Variable 1)
                        (Abstraction $ Variable 0))
      @?=
      (Right $ Abstraction $ Abstraction $ Variable 0)

  , testCase "Update index of free variable on substitution" $
      eval (Application (Abstraction $ Abstraction $ Variable 1)
                        (Abstraction $ Variable 1))
      @?=
      (Right $ Abstraction $ Abstraction $ Variable 2)

  , testCase "Evaluates sub terms before application" $
    eval (Application (Application (Abstraction $ Variable 0) (Abstraction $ Variable 0) )
                      (Application (Abstraction $ Variable 0) (Abstraction $ Variable 0) ))
    @?=
    (Right $ Abstraction $ Variable 0)
  ]
