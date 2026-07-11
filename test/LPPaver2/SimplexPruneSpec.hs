module LPPaver2.SimplexPruneSpec (spec) where

import AERN2.MP qualified as MP
import Data.Map qualified as Map
import GHC.Records (HasField (getField))
import LPPaver2.LinearPrune (LinearPruneResult (..))
import LPPaver2.RealConstraints.Boxes (Box (..), Box_ (..), mkBox)
import LPPaver2.RealConstraints.Expr (Expr (..), exprLit, exprVar)
import LPPaver2.SimplexPrune (simplexPrune)
import MixedTypesNumPrelude
import Test.Hspec
import Prelude qualified as P

x :: Expr
x = exprVar "x"

y :: Expr
y = exprVar "y"

spec :: Spec
spec = describe "simplexPrune" $ do
  it "uses simplex with shifted domains without colliding fresh variables" $ do
    let form = 3.0 * x - y <= exprLit 0.0
        box = mkBox [("x", (-1.0, 10.0)), ("y", (0.0, 6.0))]

    assertPrunedUpperBound "x" 2.0 =<< simplexPrune box form Map.empty

  it "uses interval fallback for unsupported nonlinear terms" $ do
    let sinX = sin x
        form = sinX + y <= exprLit 1.0
        box = mkBox [("x", (0.0, 1.0)), ("y", (0.0, 1.0))]
        exprBounds =
          Map.fromList
            [ (sinX.root, (-1.0, 1.0))
            ]

    result <- simplexPrune box form exprBounds
    case result of
      Nothing -> pure ()
      Just _ -> expectationFailure "expected no simplex pruning improvement"

assertPrunedUpperBound :: String -> Rational -> Maybe LinearPruneResult -> Expectation
assertPrunedUpperBound var expectedUpper result =
  case result of
    Just LinearPruneResult {maybeRemainingBox = Just remainingBox} ->
      case Map.lookup var remainingBox.box_.varDomains of
        Nothing -> expectationFailure $ "missing variable " <> var <> " in remaining box"
        Just domain -> do
          let (_lo, hi) = MP.endpoints domain
              tolerance = 1e-20 :: Rational
              actualUpper = rational hi
          P.abs (actualUpper - expectedUpper) `shouldSatisfy` (\delta -> delta P.<= tolerance)
    Just LinearPruneResult {maybeRemainingBox = Nothing} ->
      expectationFailure "expected a pruned remaining box, got infeasible"
    Nothing ->
      expectationFailure "expected simplex pruning to tighten the box"
