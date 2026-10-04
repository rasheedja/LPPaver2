module LPPaver2.RealConstraints.FormSpec (spec) where

import Control.Exception (evaluate)
import Data.Map qualified as Map
import Data.Set qualified as Set
import GHC.Records (HasField (getField))
import LPPaver2.RealConstraints.Expr
import LPPaver2.RealConstraints.Form
import MixedTypesNumPrelude
import System.Timeout (timeout)
import Test.Hspec
import Prelude qualified as P

spec :: Spec
spec = describe "formVariables" $ do
  it "collects variables from comparison operands and expression node kinds" $ do
    let expression = expr2 OpPlus (expr1 OpNeg (exprVar "x")) (expr2 OpTimes (exprVar "y") (exprLit 2.0))
        form = formComp CompLeq expression (exprVar "z")
    formVariables form `shouldBe` Set.fromList ["x", "y", "z"]

  it "visits unary and binary forms and every if-then-else branch" $ do
    let comparison var = formComp CompLeq (exprVar var) (exprLit 0.0)
        condition = form1 ConnNeg (comparison "condition")
        trueBranch = form2 ConnAnd (comparison "true") formTrue
        falseBranch = form2 ConnImpl formFalse (form2 ConnOr (comparison "left") (comparison "right"))
        form = formIfThenElse condition trueBranch falseBranch
    formVariables form `shouldBe` Set.fromList ["condition", "true", "left", "right"]

  it "ignores expression and form nodes unreachable from the root" $ do
    let form = formComp CompLeq (exprVar "reachable") (exprLit 0.0)
        unusedExpression = exprVar "unused-expression"
        unusedForm = formComp CompEq (exprVar "unused-form") (exprLit 1.0)
        withUnusedNodes =
          form
            { nodesE = Map.union form.nodesE unusedExpression.nodes,
              nodesF = Map.union form.nodesF unusedForm.nodesF
            }
    formVariables withUnusedNodes `shouldBe` Set.singleton "reachable"

  it "still rejects a missing reachable form hash" $ do
    let form = form1 ConnNeg formTrue
        missingChild = form {nodesF = Map.delete formTrue.root form.nodesF}
    evaluate (Set.size (formVariables missingChild)) `shouldThrow` errorCall "Missing node in formula"

  it "still rejects a missing reachable expression hash" $ do
    let x = exprVar "x"
        form = formComp CompLeq (expr1 OpNeg x) (exprLit 0.0)
        missingChild = form {nodesE = Map.delete x.root form.nodesE}
    evaluate (Set.size (formVariables missingChild)) `shouldThrow` errorCall "A hash is missing from form.nodesE"

  it "visits a deeply shared expression DAG without expanding every path" $ do
    let expression = P.iterate (\e -> expr2 OpPlus e e) (exprVar "x") P.!! int 32
        form = formComp CompLeq expression (exprLit 0.0)
    completesWithX form

  it "visits a deeply shared form DAG without expanding every path" $ do
    let comparison = formComp CompLeq (exprVar "x") (exprLit 0.0)
        form = P.iterate (\f -> form2 ConnAnd f f) comparison P.!! int 32
    completesWithX form

completesWithX :: Form -> Expectation
completesWithX form = do
  result <- timeout (int 5000000) $ evaluate (formVariables form P.== Set.singleton "x")
  result `shouldBe` Just True
