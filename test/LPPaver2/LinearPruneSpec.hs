module LPPaver2.LinearPruneSpec (spec) where

import AERN2.MP qualified as MP
import AERN2.MP.Affine (MPAffine (MPAffine), MPAffineConfig (..))
import BranchAndPrune.BranchAndPrune qualified as BP
import Data.Map qualified as Map
import GHC.Records (HasField (getField))
import LPPaver2.LinearPrune
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
import LPPaver2.RealConstraints.Expr
import LPPaver2.RealConstraints.Form
import MixedTypesNumPrelude
import Test.Hspec
import Prelude qualified as P

x :: Expr
x = exprVar "x"

y :: Expr
y = exprVar "y"

lit1 :: Expr
lit1 = exprLit 1.0

lit2 :: Expr
lit2 = exprLit 2.0

sampleMPBall :: MP.MPBall
sampleMPBall = MP.mpBallP (MP.prec 1000) 0

sampleMPAffine :: MPAffine
sampleMPAffine = MPAffine config (convertExactly 0) Map.empty
  where
    config = MPAffineConfig {maxTerms = int 10, precision = 1000}

aaBoundTolerance :: Rational
aaBoundTolerance = 1e-12

spec :: Spec
spec = do
  describe "extractCIEorDIE" $ do
    it "extracts x <= 1 as IE" $ do
      extractCIEorDIE (x <= lit1) `shouldBe` Just (x <= lit1, IE)

    it "extracts x == 1 as CIE" $ do
      extractCIEorDIE (x == lit1) `shouldBe` Just (x == lit1, CIE)

    it "extracts x /= y as DIE" $ do
      extractCIEorDIE (x /= y) `shouldBe` Just (x /= y, DIE)

    it "extracts conjunction of two IEs as CIE" $ do
      let form1 = x <= lit1
          form2 = y <= lit2
      extractCIEorDIE (form1 && form2) `shouldBe` Just (form1 && form2, CIE)

    it "extracts disjunction of two IEs as DIE" $ do
      let form1 = x <= lit1
          form2 = y <= lit2
      extractCIEorDIE (form1 || form2) `shouldBe` Just (form1 || form2, DIE)

    it "returns Nothing for non-inequality constraints" $ do
      extractCIEorDIE formTrue `shouldBe` Nothing

    it "extracts IE from conjunction with non-inequality (True)" $ do
      let form1 = x <= lit1
      extractCIEorDIE (form1 && formTrue) `shouldBe` Just (form1, CIE)

    it "extracts IE from conjunction with non-inequality (False)" $ do
      let form1 = x <= lit1
      extractCIEorDIE (formFalse && form1) `shouldBe` Just (form1, CIE)

    it "extracts IE from disjunction with non-inequality (True)" $ do
      let form1 = x <= lit1
      extractCIEorDIE (form1 || formTrue) `shouldBe` Just (form1, DIE)

    it "extracts IE from disjunction with non-inequality (False)" $ do
      let form1 = x <= lit1
      extractCIEorDIE (formFalse || form1) `shouldBe` Just (form1, DIE)

    it "extracts CIE from conjunction of CIE and non-inequality" $ do
      let form1 = x <= lit1
          form2 = y <= lit2
          cieForm = form1 && form2
      extractCIEorDIE (cieForm && formTrue) `shouldBe` Just (cieForm, CIE)

    it "extracts DIE from disjunction of DIE and non-inequality" $ do
      let form1 = x <= lit1
          form2 = y <= lit2
          dieForm = form1 || form2
      extractCIEorDIE (formFalse || dieForm) `shouldBe` Just (dieForm, DIE)

    it "extracts cie1 && cie2 from ((nonie && cie1) && (nonie && cie2))" $ do
      let cie1 = x <= lit1
          cie2 = y <= lit2
          nonie = formTrue
      extractCIEorDIE ((nonie && cie1) && (nonie && cie2)) `shouldBe` Just (cie1 && cie2, CIE)

    it "extracts cie2 from ((nonie || cie1) && (nonie && cie2))" $ do
      let cie1 = x <= lit1
          cie2 = y <= lit2
          nonie = formTrue
      extractCIEorDIE ((nonie || cie1) && (nonie && cie2)) `shouldBe` Just (cie2, CIE)

  describe "linearPruneWithEvalBounds" $ do
    it "uses affine arithmetic fallback bounds when available" $ do
      let form = sin (x - x) + y <= exprLit 0.5
          box = mkBox [("x", (0.0, 1.0)), ("y", (0.0, 1.0))]

      let iaResult = linearPruneAfterSimplify sampleMPBall box form
          aaResult = linearPruneAfterSimplify sampleMPAffine box form

      expectNoPruning iaResult
      assertPrunedLowerBoundWithin aaBoundTolerance "y" 0.0 aaResult
      assertPrunedUpperBoundWithin aaBoundTolerance "y" 0.5 aaResult

    it "uses affine arithmetic fallback bounds to tighten lower bounds" $ do
      let form = exprLit 0.5 <= sin (x - x) + y
          box = mkBox [("x", (0.0, 1.0)), ("y", (0.0, 1.0))]

      let iaResult = linearPruneAfterSimplify sampleMPBall box form
          aaResult = linearPruneAfterSimplify sampleMPAffine box form

      expectNoPruning iaResult
      assertPrunedLowerBoundWithin aaBoundTolerance "y" 0.5 aaResult
      assertPrunedUpperBoundWithin aaBoundTolerance "y" 1.0 aaResult

    it "detects constant-false conjuncts" $ do
      let form = (x <= exprLit 0.0) && (exprLit 1.0 <= exprLit 0.0)
          box = mkBox [("x", (0.0, 1.0))]

      assertInfeasible $ linearPrune BP.Problem {scope = box, constraint = form}

linearPruneAfterSimplify ::
  (CanEval r, HasKleeneanComparison r, ConvertibleExactly r MP.MPBall) =>
  r ->
  Box ->
  Form ->
  Maybe LinearPruneResult
linearPruneAfterSimplify sampleR box form =
  linearPruneWithEvalBounds BP.Problem {scope = box, constraint = simplifiedForm} exprValues
  where
    simplificationResult = simplifyEvalForm sampleR box form
    simplifiedForm = simplificationResult.evaluatedForm.form
    exprValues = simplificationResult.evaluatedForm.exprValues

expectNoPruning :: Maybe LinearPruneResult -> Expectation
expectNoPruning result =
  case result of
    Nothing -> pure ()
    Just _ -> expectationFailure "expected linear pruning not to tighten the box"

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
      expectationFailure "expected linear pruning to tighten the box"

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
      expectationFailure "expected linear pruning to tighten the box"

assertInfeasible :: Maybe LinearPruneResult -> Expectation
assertInfeasible result =
  case result of
    Just LinearPruneResult {maybeRemainingBox = Nothing, removedRegionTruth = False} -> pure ()
    Just LinearPruneResult {maybeRemainingBox = Nothing, removedRegionTruth = True} ->
      expectationFailure "expected constant-false constraint to remove an outer region"
    Just LinearPruneResult {maybeRemainingBox = Just _} -> expectationFailure "expected infeasible pruning result"
    Nothing -> expectationFailure "expected pruning to detect infeasibility"

assertNear :: Rational -> Rational -> Rational -> Expectation
assertNear expected actual tolerance =
  P.abs (actual - expected) `shouldSatisfy` (\delta -> delta P.<= tolerance)
