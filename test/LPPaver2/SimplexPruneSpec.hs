module LPPaver2.SimplexPruneSpec (spec) where

import AERN2.MP qualified as MP
import AERN2.MP.Affine (MPAffine (MPAffine), MPAffineConfig (..))
import Data.Map qualified as Map
import GHC.Records (HasField (getField))
import LPPaver2.LinearPrune (LinearPruneResult (..))
import LPPaver2.RealConstraints.Boxes (Box (..), Box_ (..), mkBox)
import LPPaver2.RealConstraints.Eval
  ( CanEval,
    EvaluatedForm (..),
    HasKleeneanComparison,
    SimplifyFormResult (..),
    simplifyEvalForm,
  )
import LPPaver2.RealConstraints.EvalArithmetic.AffArith ()
import LPPaver2.RealConstraints.EvalArithmetic.MPBall ()
import LPPaver2.RealConstraints.Expr (Expr (..), exprLit, exprVar)
import LPPaver2.RealConstraints.Form (Form)
import LPPaver2.SimplexPrune (simplexPrune, simplexPruneWithEvalBounds)
import MixedTypesNumPrelude
import Test.Hspec
import Prelude qualified as P

x :: Expr
x = exprVar "x"

y :: Expr
y = exprVar "y"

sampleMPBall :: MP.MPBall
sampleMPBall = MP.mpBallP (MP.prec 1000) 0

sampleMPAffine :: MPAffine
sampleMPAffine = MPAffine config (convertExactly 0) Map.empty
  where
    config = MPAffineConfig {maxTerms = int 10, precision = 1000}

exactBoundTolerance :: Rational
exactBoundTolerance = 1e-20

aaBoundTolerance :: Rational
aaBoundTolerance = 1e-12

spec :: Spec
spec = describe "simplexPrune" $ do
  it "uses simplex with shifted domains without colliding fresh variables" $ do
    let form = 3.0 * x - y <= exprLit 0.0
        box = mkBox [("x", (-1.0, 10.0)), ("y", (0.0, 6.0))]

    assertPrunedUpperBound "x" 2.0 =<< simplexPrune box form Map.empty

  it "decomposes division by a positive literal exactly" $ do
    let form = x / exprLit 2.0 + y <= exprLit 1.0
        box = mkBox [("x", (0.0, 4.0)), ("y", (0.0, 4.0))]

    result <- simplexPrune box form Map.empty

    assertPrunedUpperBound "x" 2.0 result
    assertPrunedUpperBound "y" 1.0 result

  it "decomposes division by a negative literal exactly" $ do
    let form = x / exprLit (-2.0) + y <= exprLit (-1.0)
        box = mkBox [("x", (0.0, 4.0)), ("y", (0.0, 4.0))]

    result <- simplexPrune box form Map.empty

    assertPrunedLowerBound "x" 2.0 result

  it "uses affine arithmetic fallback bounds when available" $ do
    let form = sin (x - x) + y <= exprLit 0.5
        box = mkBox [("x", (0.0, 1.0)), ("y", (0.0, 1.0))]

    iaResult <- simplexPruneAfterSimplify sampleMPBall box form
    aaResult <- simplexPruneAfterSimplify sampleMPAffine box form

    expectNoPruning iaResult
    assertPrunedLowerBoundWithin aaBoundTolerance "y" 0.0 aaResult
    assertPrunedUpperBoundWithin aaBoundTolerance "y" 0.5 aaResult

  it "detects contradictory linear constraints" $ do
    let form = (x <= exprLit 0.0) && (exprLit 1.0 <= x)
        box = mkBox [("x", (0.0, 1.0))]

    result <- simplexPrune box form Map.empty

    assertInfeasible result

  it "detects constant-false conjuncts" $ do
    let form = (x <= exprLit 0.0) && (exprLit 1.0 <= exprLit 0.0)
        box = mkBox [("x", (0.0, 1.0))]

    result <- simplexPrune box form Map.empty

    assertInfeasible result

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

simplexPruneAfterSimplify ::
  (CanEval r, HasKleeneanComparison r, ConvertibleExactly r MP.MPBall) =>
  r ->
  Box ->
  Form ->
  IO (Maybe LinearPruneResult)
simplexPruneAfterSimplify sampleR box form =
  simplexPruneWithEvalBounds box simplifiedForm exprValues
  where
    simplificationResult = simplifyEvalForm sampleR box form
    simplifiedForm = simplificationResult.evaluatedForm.form
    exprValues = simplificationResult.evaluatedForm.exprValues

expectNoPruning :: Maybe LinearPruneResult -> Expectation
expectNoPruning result =
  case result of
    Nothing -> pure ()
    Just _ -> expectationFailure "expected simplex pruning not to tighten the box"

assertPrunedUpperBound :: String -> Rational -> Maybe LinearPruneResult -> Expectation
assertPrunedUpperBound = assertPrunedUpperBoundWithin exactBoundTolerance

assertPrunedUpperBoundWithin :: Rational -> String -> Rational -> Maybe LinearPruneResult -> Expectation
assertPrunedUpperBoundWithin tolerance var expectedUpper result =
  case result of
    Just LinearPruneResult {maybeRemainingBox = Just remainingBox} ->
      case Map.lookup var remainingBox.box_.varDomains of
        Nothing -> expectationFailure $ "missing variable " <> var <> " in remaining box"
        Just domain -> do
          let (_lo, hi) = MP.endpoints domain
          assertNear expectedUpper (rational hi) tolerance
    Just LinearPruneResult {maybeRemainingBox = Nothing} ->
      expectationFailure "expected a pruned remaining box, got infeasible"
    Nothing ->
      expectationFailure "expected simplex pruning to tighten the box"

assertPrunedLowerBound :: String -> Rational -> Maybe LinearPruneResult -> Expectation
assertPrunedLowerBound = assertPrunedLowerBoundWithin exactBoundTolerance

assertPrunedLowerBoundWithin :: Rational -> String -> Rational -> Maybe LinearPruneResult -> Expectation
assertPrunedLowerBoundWithin tolerance var expectedLower result =
  case result of
    Just LinearPruneResult {maybeRemainingBox = Just remainingBox} ->
      case Map.lookup var remainingBox.box_.varDomains of
        Nothing -> expectationFailure $ "missing variable " <> var <> " in remaining box"
        Just domain -> do
          let (lo, _hi) = MP.endpoints domain
          assertNear expectedLower (rational lo) tolerance
    Just LinearPruneResult {maybeRemainingBox = Nothing} ->
      expectationFailure "expected a pruned remaining box, got infeasible"
    Nothing ->
      expectationFailure "expected simplex pruning to tighten the box"

assertInfeasible :: Maybe LinearPruneResult -> Expectation
assertInfeasible result =
  case result of
    Just LinearPruneResult {maybeRemainingBox = Nothing, removedRegionTruth = False} -> pure ()
    Just LinearPruneResult {maybeRemainingBox = Nothing, removedRegionTruth = True} ->
      expectationFailure "expected infeasible constraint to remove an outer region"
    Just LinearPruneResult {maybeRemainingBox = Just _} -> expectationFailure "expected infeasible pruning result"
    Nothing -> expectationFailure "expected simplex pruning to detect infeasibility"

assertNear :: Rational -> Rational -> Rational -> Expectation
assertNear expected actual tolerance =
  P.abs (actual - expected) `shouldSatisfy` (\delta -> delta P.<= tolerance)
