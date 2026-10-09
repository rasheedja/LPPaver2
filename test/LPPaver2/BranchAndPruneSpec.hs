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

  P.mapM_
    (\useSimplex ->
       P.mapM_
         (\lower ->
            it ("retains feasible points with independent sine errors, simplex=" P.++ show useSimplex P.++ ", lower=" P.++ show lower) $ do
              let z = exprVar "z"
                  box = mkBox [("x", (lower, 2.0)), ("y", (0.0, 8.0)), ("z", (0.0, 8.0))]
                  form = x + sin y <= sin z
                  -- (1.25, 4, 1) satisfies the constraint. Equal sine ranges
                  -- must not make unrelated variables' uncertainty cancel.
              paving <- pruneWith (affineMethod useSimplex) box form
              assertRemainingBoxUnchanged box paving
         )
         [0.0, 1.0]
    )
    [False, True]

  P.mapM_
    (\(arithmeticLabel, arithmeticMethod) ->
       P.mapM_
         (\useSimplex ->
            P.mapM_
              (\lower ->
                 it ("retains feasible points with independent reciprocal errors, " P.++ arithmeticLabel P.++ ", simplex=" P.++ show useSimplex P.++ ", lower=" P.++ show lower) $ do
                   let z = exprVar "z"
                       box = mkBox [("x", (lower, 0.5)), ("y", (1.0, 2.0)), ("z", (1.0, 2.0))]
                       form = x + exprLit 1.0 / y <= exprLit 1.0 / z && y <= exprLit 1.5
                       -- (0.3, 1.5, 1) satisfies both constraints. Reciprocals of
                       -- unrelated variables with matching ranges must not cancel
                       -- their uncertainty, which previously collapsed x onto 0.
                       feasiblePoint = [("x", 0.3), ("y", 1.5), ("z", 1.0)]
                   paving <- pruneWith (arithmeticMethod useSimplex) box form
                   assertRetainsPoint feasiblePoint paving
              )
              [0.0, 0.3]
         )
         [False, True]
    )
    [("IA", mpBallMethod), ("AA", affineMethod)]

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

-- | Assert that pruning kept a given feasible point: it must lie inside the
-- undecided or inner region and must not lie inside any excluded outer box.
assertRetainsPoint :: [(String, Rational)] -> BP.Paving Form Box Boxes -> Expectation
assertRetainsPoint point paving = do
  let containsPoint domains =
        P.all
          (\(var, value) ->
             case Map.lookup var domains of
               Nothing -> False
               Just domain ->
                 let (lower, upper) = MP.endpoints domain
                  in rational lower P.<= value P.&& value P.<= rational upper
          )
          point
      pointInside box =
        containsPoint box.box_.varDomains
          P.&& P.not (P.maybe False containsPoint box.box_.except)
      undecidedBoxes = [scope | BP.Problem {scope} <- paving.undecided]

  P.any pointInside (Map.elems paving.inner.store P.++ undecidedBoxes) `shouldBe` True
  P.any pointInside (Map.elems paving.outer.store) `shouldBe` False
