{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}

module LPPaver2.PruneSpecSupport
  ( x,
    y,
    sampleMPBall,
    sampleMPAffine,
    exactBoundTolerance,
    aaBoundTolerance,
    simplifiedFormAndValues,
    expectNoPruning,
    assertPrunedUpperBound,
    assertPrunedUpperBoundWithin,
    assertPrunedLowerBound,
    assertPrunedLowerBoundWithin,
    assertInfeasible,
  )
where

import AERN2.MP qualified as MP
import AERN2.MP (mpBallP)
import AERN2.MP.Affine (MPAffine (MPAffine), MPAffineConfig (..))
import Data.Map qualified as Map
import GHC.Records (HasField (getField))
import LPPaver2.LinearPrune (LinearPruneResult (..))
import LPPaver2.RealConstraints.Boxes (Box (..), Box_ (..))
import LPPaver2.RealConstraints.Eval
  ( CanEval,
    EvaluatedFormR (..),
    HasKleeneanComparison,
    SimplifyFormResultR (..),
    simplifyEvalFormR,
  )
import LPPaver2.RealConstraints.Expr (Expr, ExprHash, exprVar)
import LPPaver2.RealConstraints.Form (Form)
import MixedTypesNumPrelude
import Test.Hspec (Expectation, expectationFailure, shouldSatisfy)
import Prelude qualified as P

x :: Expr
x = exprVar "x"

y :: Expr
y = exprVar "y"

exactBoundTolerance :: Rational
exactBoundTolerance = 1e-20

aaBoundTolerance :: Rational
aaBoundTolerance = 1e-12

sampleMPBall :: MP.MPBall
sampleMPBall = mpBallP (MP.prec 1000) 0

sampleMPAffine :: MPAffine
sampleMPAffine = MPAffine config (convertExactly 0) Map.empty
  where
    config :: MPAffineConfig
    config = MPAffineConfig {maxTerms = int 10, precision = 1000}

simplifiedFormAndValues ::
  (CanEval r, HasKleeneanComparison r) =>
  r ->
  Box ->
  Form ->
  (Form, Map.Map ExprHash r)
simplifiedFormAndValues sampleR box form =
  (simplified.form, simplified.exprValues)
  where
    SimplifyFormResultR {evaluatedForm = simplified} = simplifyEvalFormR sampleR box form

expectNoPruning :: Maybe LinearPruneResult -> Expectation
expectNoPruning result =
  case result of
    Nothing -> pure ()
    Just _ -> expectationFailure "expected pruning not to tighten the box"

assertPrunedUpperBound :: String -> Rational -> Maybe LinearPruneResult -> Expectation
assertPrunedUpperBound = assertPrunedUpperBoundWithin exactBoundTolerance

assertPrunedUpperBoundWithin :: Rational -> String -> Rational -> Maybe LinearPruneResult -> Expectation
assertPrunedUpperBoundWithin = assertPrunedBoundWithin P.snd

assertPrunedLowerBound :: String -> Rational -> Maybe LinearPruneResult -> Expectation
assertPrunedLowerBound = assertPrunedLowerBoundWithin exactBoundTolerance

assertPrunedLowerBoundWithin :: Rational -> String -> Rational -> Maybe LinearPruneResult -> Expectation
assertPrunedLowerBoundWithin = assertPrunedBoundWithin P.fst

assertPrunedBoundWithin :: ((Rational, Rational) -> Rational) -> Rational -> String -> Rational -> Maybe LinearPruneResult -> Expectation
assertPrunedBoundWithin selectBound tolerance var expected result =
  case result of
    Just LinearPruneResult {maybeRemainingBox = Just remainingBox} ->
      case Map.lookup var remainingBox.box_.varDomains of
        Nothing -> expectationFailure $ "missing variable " <> var <> " in remaining box"
        Just domain -> do
          let (lower, upper) = MP.endpoints domain
              actual = selectBound (rational lower, rational upper)
          P.abs (actual - expected) `shouldSatisfy` (P.<= tolerance)
    Just LinearPruneResult {maybeRemainingBox = Nothing} ->
      expectationFailure "expected a pruned remaining box, got infeasible"
    Nothing ->
      expectationFailure "expected pruning to tighten the box"

assertInfeasible :: Maybe LinearPruneResult -> Expectation
assertInfeasible result =
  case result of
    Just LinearPruneResult {maybeRemainingBox = Nothing, removedRegionTruth = False} -> pure ()
    Just LinearPruneResult {maybeRemainingBox = Nothing, removedRegionTruth = True} ->
      expectationFailure "expected infeasible constraint to remove an outer region"
    Just LinearPruneResult {maybeRemainingBox = Just _} ->
      expectationFailure "expected infeasible pruning result"
    Nothing -> expectationFailure "expected pruning to detect infeasibility"
