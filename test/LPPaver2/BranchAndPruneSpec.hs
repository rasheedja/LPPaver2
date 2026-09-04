{-# LANGUAGE OverloadedRecordDot #-}

module LPPaver2.BranchAndPruneSpec (spec) where

import AERN2.MP qualified as MP
import BranchAndPrune.BranchAndPrune qualified as BP
import Data.Map qualified as Map
import GHC.Records (HasField (getField))
import LPPaver2.BranchAndPrune (LPPPruningMethod (..))
import LPPaver2.PruneSpecSupport
  ( aaBoundTolerance,
    sampleMPAffine,
    sampleMPBall,
    x,
    y,
  )
import LPPaver2.RealConstraints
import MixedTypesNumPrelude
import Test.Hspec
import Prelude qualified as P

spec :: Spec
spec = describe "runtime pruning dispatch" $ do
  it "uses existing IA linear pruning" $ do
    paving <- pruneWith (mpBallMethod False) (mkBox [("x", (0.0, 10.0))]) (x <= exprLit 5.0)

    assertRemainingUpperBound "x" 5.0 paving

  it "uses evaluated affine values for ordinary AA pruning" $ do
    let box = mkBox [("x", (0.0, 1.0)), ("y", (0.0, 1.0))]
        form = sin (x - x) + y <= exprLit 0.5

    paving <- pruneWith (affineMethod False) box form

    assertRemainingUpperBoundWithin aaBoundTolerance "y" 0.5 paving

  it "dispatches IA expression values to simplex pruning" $ do
    let box = mkBox [("x", (1.0, 2.0))]
        form = x * x <= exprLit 2.0

    basicPaving <- pruneWith (mpBallMethod False) box form
    assertRemainingBoxUnchanged box basicPaving

    paving <- pruneWith (mpBallMethod True) box form

    assertRemainingUpperBoundWithin aaBoundTolerance "x" 1.5 paving

  it "uses coupled AA simplex relaxations unavailable to basic pruning" $ do
    let box = mkBox [("x", (0.0, 10.0)), ("y", (0.0, 10.0))]
        form = x + y <= exprLit 5.0

    basicPaving <- pruneWith (affineMethod False) box form
    assertRemainingBoxUnchanged box basicPaving

    paving <- pruneWith (affineMethod True) box form

    assertRemainingUpperBoundWithin aaBoundTolerance "x" 5.0 paving
    assertRemainingUpperBoundWithin aaBoundTolerance "y" 5.0 paving

  it "preserves fixed parameters during runtime pruning" $ do
    let baseBox = mkBox [("x", (0.0, 10.0))]
        box = addParamValuesToBox (Map.fromList [("p", 1.0)]) baseBox
        form = x + exprVar "p" <= exprLit 5.0

    paving <- pruneWith (mpBallMethod True) box form

    assertRemainingUpperBound "x" 4.0 paving
    case remainingBox paving of
      Nothing -> expectationFailure "expected an undecided remaining box"
      Just box' -> do
        let domains = box'.box_.varDomains
        MP.endpoints (domains Map.! "p")
          `shouldSatisfy` (\(lower, upper) -> rational lower == 1 && rational upper == 1)
        box'.box_.volumeVars `shouldBe` box.box_.volumeVars

mpBallMethod :: Bool -> LPPPruningMethod
mpBallMethod useSimplex =
  LPPPruningMethod
    { evalArithmetic = EvalArithmeticMPBall {sampleBall = sampleMPBall},
      useSimplex
    }

affineMethod :: Bool -> LPPPruningMethod
affineMethod useSimplex =
  LPPPruningMethod
    { evalArithmetic = EvalArithmeticAffine {sampleAffine = sampleMPAffine},
      useSimplex
    }

pruneWith :: LPPPruningMethod -> Box -> Form -> IO (BP.Paving Form Box Boxes)
pruneWith pruningMethod scope constraint = do
  (paving, _) <-
    ( BP.pruneProblemM pruningMethod BP.Problem {scope, constraint} ::
        IO (BP.Paving Form Box Boxes, EvaluatedForm)
      )
  pure paving

remainingBox :: BP.Paving Form Box Boxes -> Maybe Box
remainingBox paving =
  case paving.undecided of
    [BP.Problem {scope}] -> Just scope
    _ -> Nothing

assertRemainingUpperBound :: String -> Rational -> BP.Paving Form Box Boxes -> Expectation
assertRemainingUpperBound = assertRemainingUpperBoundWithin 1e-20

assertRemainingUpperBoundWithin :: Rational -> String -> Rational -> BP.Paving Form Box Boxes -> Expectation
assertRemainingUpperBoundWithin tolerance var expected paving =
  case remainingBox paving of
    Nothing -> expectationFailure "expected a tightened remaining box"
    Just box ->
      case Map.lookup var box.box_.varDomains of
        Nothing -> expectationFailure $ "missing variable " <> var <> " in remaining box"
        Just domain -> do
          let (_, upper) = MP.endpoints domain
          P.abs (rational upper - expected) `shouldSatisfy` (P.<= tolerance)

assertRemainingBoxUnchanged :: Box -> BP.Paving Form Box Boxes -> Expectation
assertRemainingBoxUnchanged original paving =
  case remainingBox paving of
    Nothing -> expectationFailure "expected an undecided remaining box"
    Just remaining -> do
      let originalDomains = original.box_.varDomains
          remainingDomains = remaining.box_.varDomains
          sameDomain (var, originalDomain) =
            case Map.lookup var remainingDomains of
              Nothing -> False
              Just remainingDomain ->
                let (originalLower, originalUpper) = MP.endpoints originalDomain
                    (remainingLower, remainingUpper) = MP.endpoints remainingDomain
                 in rational originalLower == rational remainingLower
                      && rational originalUpper == rational remainingUpper
      P.all sameDomain (Map.toList originalDomains) `shouldBe` True
      remaining.box_.volumeVars `shouldBe` original.box_.volumeVars
