module LPPaver2.SimplexPruneSpec (spec) where

import AERN2.MP qualified as MP
import Data.Map qualified as Map
import GHC.Records (HasField (getField))
import LPPaver2.LinearPrune (LinearPruneResult (..))
import LPPaver2.PruneSpecSupport
import LPPaver2.RealConstraints.Boxes (Box (..), Box_ (..), addParamValuesToBox, mkBox)
import LPPaver2.RealConstraints.Eval
  ( CanEval,
    HasKleeneanComparison,
  )
import LPPaver2.RealConstraints.EvalArithmetic.AffArith ()
import LPPaver2.RealConstraints.EvalArithmetic.MPBall ()
import LPPaver2.RealConstraints.Expr (ExprHash, exprLit, exprVar)
import LPPaver2.RealConstraints.Form (Form)
import LPPaver2.SimplexPrune (CanProvideSimplexRelaxations, simplexPrune, simplexPruneWithEvalValues)
import MixedTypesNumPrelude
import Test.Hspec
import Test.QuickCheck (Property, chooseInteger, conjoin, counterexample, forAll, ioProperty, property)
import Prelude qualified as P

spec :: Spec
spec = describe "simplexPrune" $ do
  it "uses simplex with shifted domains without colliding fresh variables" $ do
    let form = 3.0 * x - y <= exprLit 0.0
        box = mkBox [("x", (-1.0, 10.0)), ("y", (0.0, 6.0))]

    assertPrunedUpperBound "x" 2.0 =<< simplexPrune box form noExprValues

  it "decomposes division by a positive literal exactly" $ do
    let form = x / exprLit 2.0 + y <= exprLit 1.0
        box = mkBox [("x", (0.0, 4.0)), ("y", (0.0, 4.0))]

    result <- simplexPrune box form noExprValues

    assertPrunedUpperBound "x" 2.0 result
    assertPrunedUpperBound "y" 1.0 result

  it "decomposes division by a negative literal exactly" $ do
    let form = x / exprLit (-2.0) + y <= exprLit (-1.0)
        box = mkBox [("x", (0.0, 4.0)), ("y", (0.0, 4.0))]

    result <- simplexPrune box form noExprValues

    assertPrunedLowerBound "x" 2.0 result

  it "tightens a box containing a relevant fixed-radius variable" $ do
    let form = x + y <= exprLit 5.0
        box = mkBox [("x", (0.0, 0.0)), ("y", (0.0, 10.0))]

    result <- simplexPrune box form noExprValues

    assertPrunedUpperBound "y" 5.0 result

  it "optimises only variables used by the constraint" $ do
    let form = y <= exprLit 5.0
        box = mkBox [("x", (0.0, 0.0)), ("y", (0.0, 10.0))]

    result <- simplexPrune box form noExprValues

    assertPrunedUpperBound "y" 5.0 result

  it "retains fixed parameter variables used by constraints" $ do
    let baseBox = mkBox [("x", (0.0, 10.0))]
        box = addParamValuesToBox (Map.fromList [("p", 1.0)]) baseBox
        form = x + exprVar "p" <= exprLit 5.0

    result <- simplexPrune box form noExprValues

    assertPrunedUpperBound "x" 4.0 result
    case result of
      Just LinearPruneResult {maybeRemainingBox = Just remainingBox} -> do
        let domains = remainingBox.box_.varDomains
        MP.endpoints (domains Map.! "p") `shouldSatisfy` (\(lower, upper) -> rational lower == 1 && rational upper == 1)
        remainingBox.box_.volumeVars `shouldBe` box.box_.volumeVars
      _ -> expectationFailure "expected a tightened box"

  it "linearises a shared expression DAG without expanding it as a tree" $ do
    let levels = 20
        sharedExpression = P.foldl (\expression _ -> expression + expression) x [1 .. levels]
        coefficient = rational (2 P.^ levels :: Integer)
        form = sharedExpression <= exprLit coefficient
        box = mkBox [("x", (0.0, 2.0))]

    result <- simplexPrune box form noExprValues

    assertPrunedUpperBound "x" 1.0 result

  it "uses IA corner and AA affine relaxations for nonlinear expressions" $ do
    let form = x * x <= exprLit 2.0
        box = mkBox [("x", (1.0, 2.0))]

    iaResult <- simplexPruneAfterSimplify sampleMPBall box form
    aaResult <- simplexPruneAfterSimplify sampleMPAffine box form

    assertPrunedUpperBoundWithin aaBoundTolerance "x" 1.5 iaResult
    assertPrunedUpperBoundWithin aaBoundTolerance "x" 1.5 aaResult

  it "uses interval derivative cancellation as well as affine cancellation" $ do
    let form = sin (x - x) + y <= exprLit 0.5
        box = mkBox [("x", (0.0, 1.0)), ("y", (0.0, 1.0))]

    iaResult <- simplexPruneAfterSimplify sampleMPBall box form
    aaResult <- simplexPruneAfterSimplify sampleMPAffine box form

    assertPrunedLowerBoundWithin aaBoundTolerance "y" 0.0 iaResult
    assertPrunedUpperBoundWithin aaBoundTolerance "y" 0.5 iaResult
    assertPrunedLowerBoundWithin aaBoundTolerance "y" 0.0 aaResult
    assertPrunedUpperBoundWithin aaBoundTolerance "y" 0.5 aaResult

  it "uses the upper derivative endpoint to prune from the left corner" $ do
    let form = exprLit 2.0 <= x * x
        box = mkBox [("x", (1.0, 2.0))]

    result <- simplexPruneAfterSimplify sampleMPBall box form

    assertPrunedLowerBoundWithin aaBoundTolerance "x" 1.25 result

  it "linearises transcendental expressions from both corners" $ do
    let form = sin x <= exprLit 0.5
        box = mkBox [("x", (0.0, 1.0))]
        expectedUpper = 0.6585290151921035

    result <- simplexPruneAfterSimplify sampleMPBall box form

    assertPrunedUpperBoundWithin aaBoundTolerance "x" expectedUpper result

  it "propagates interval derivatives through cosine" $ do
    let form = cos x <= exprLit 0.5
        box = mkBox [("x", (0.0, 2.0))]

    result <- simplexPruneAfterSimplify sampleMPBall box form

    assertPrunedLowerBoundWithin aaBoundTolerance "x" 0.5 result

  it "combines coefficients from a multivariate corner linearisation" $ do
    let form = x * y <= exprLit 1.0
        box = mkBox [("x", (1.0, 2.0)), ("y", (1.0, 2.0))]

    result <- simplexPruneAfterSimplify sampleMPBall box form

    assertPrunedUpperBoundWithin aaBoundTolerance "x" 1.0 result
    assertPrunedUpperBoundWithin aaBoundTolerance "y" 1.0 result

  it "propagates interval derivatives through square roots" $ do
    let form = sqrt x <= exprLit 1.0
        box = mkBox [("x", (0.25, 4.0))]

    result <- simplexPruneAfterSimplify sampleMPBall box form

    assertPrunedUpperBoundWithin aaBoundTolerance "x" 2.25 result

  it "propagates interval derivatives through division" $ do
    let form = exprLit 1.0 / x <= exprLit 0.5
        box = mkBox [("x", (1.0, 4.0))]

    result <- simplexPruneAfterSimplify sampleMPBall box form

    assertPrunedLowerBoundWithin aaBoundTolerance "x" 1.5 result

  it "handles derivatives through a negative denominator" $ do
    let form = exprLit (-0.5) <= exprLit 1.0 / x
        box = mkBox [("x", (-4.0, -1.0))]

    result <- simplexPruneAfterSimplify sampleMPBall box form

    assertPrunedUpperBoundWithin aaBoundTolerance "x" (-1.5) result

  it "skips division when the denominator domain contains zero" $ do
    let form = exprLit 1.0 / x <= exprLit 0.5
        box = mkBox [("x", (-1.0, 4.0))]

    expectNoPruning =<< simplexPrune box form noExprValues

  it "skips square-root linearisation when its derivative is unbounded at zero" $ do
    let form = sqrt x <= exprLit 1.0
        box = mkBox [("x", (0.0, 4.0))]

    expectNoPruning =<< simplexPrune box form noExprValues

  it "retains feasible points when trigonometric derivatives change sign" $ do
    let form = sin x <= exprLit 0.0
        box = mkBox [("x", (0.0, 8.0))]

    result <- simplexPruneAfterSimplify sampleMPBall box form

    assertPointRetained [("x", 6.0)] result

  it "never removes generated feasible boundary points" $
    property generatedFeasiblePointsAreRetained

  it "detects contradictory linear constraints" $ do
    let form = (x <= exprLit 0.0) && (exprLit 1.0 <= x)
        box = mkBox [("x", (0.0, 1.0))]

    result <- simplexPrune box form noExprValues

    assertInfeasible result

  it "detects constant-false conjuncts" $ do
    let form = (x <= exprLit 0.0) && (exprLit 1.0 <= exprLit 0.0)
        box = mkBox [("x", (0.0, 1.0))]

    result <- simplexPrune box form noExprValues

    assertInfeasible result

  it "skips unsupported nonlinear terms without fallback bounds" $ do
    let form = sin x + y <= exprLit 1.0
        box = mkBox [("x", (0.0, 1.0)), ("y", (0.0, 1.0))]

    result <- simplexPrune box form noExprValues

    expectNoPruning result

  it "leaves a box unchanged when corner linearisations cannot improve it" $ do
    let form = sin x + y <= exprLit 1.0
        box = mkBox [("x", (0.0, 1.0)), ("y", (0.0, 1.0))]

    expectNoPruning =<< simplexPruneAfterSimplify sampleMPBall box form

simplexPruneAfterSimplify ::
  (CanEval r, HasKleeneanComparison r, CanProvideSimplexRelaxations r) =>
  r ->
  Box ->
  Form ->
  IO (Maybe LinearPruneResult)
simplexPruneAfterSimplify sampleR box form =
  simplexPruneWithEvalValues box simplifiedForm exprValues
  where
    (simplifiedForm, exprValues) = simplifiedFormAndValues sampleR box form

noExprValues :: Map.Map ExprHash MP.MPBall
noExprValues = Map.empty

generatedFeasiblePointsAreRetained :: Property
generatedFeasiblePointsAreRetained =
  forAll (chooseInteger (1, 8)) $ \pNumerator ->
    forAll (chooseInteger (1, 8)) $ \qNumerator ->
      let p = rational pNumerator P./ rational 2
          q = rational qNumerator P./ rational 2
          pSquared = p P.* p
          qSquared = q P.* q
          positiveBox = mkBox [("x", (0.0, 5.0))]
          productBox = mkBox [("x", (0.0, 5.0)), ("y", (0.0, 5.0))]
          reciprocalBox = mkBox [("x", (0.5, 5.0))]
          sqrtBox = mkBox [("x", (0.0, 16.0))]
       in ioProperty $ do
            upperSquare <-
              retentionProperty "x*x <= p^2" [("x", p)] positiveBox
                $ x * x <= exprLit pSquared
            lowerSquare <-
              retentionProperty "p^2 <= x*x" [("x", p)] positiveBox
                $ exprLit pSquared <= x * x
            productCase <-
              retentionProperty "x*y <= p*q" [("x", p), ("y", q)] productBox
                $ x * y <= exprLit (p P.* q)
            reciprocalCase <-
              retentionProperty "1/x <= 1/p" [("x", p)] reciprocalBox
                $ exprLit (rational 1) / x <= exprLit (P.recip p)
            sqrtCase <-
              retentionProperty "sqrt x <= q" [("x", qSquared)] sqrtBox
                $ sqrt x <= exprLit q
            pure $ conjoin [upperSquare, lowerSquare, productCase, reciprocalCase, sqrtCase]

retentionProperty :: String -> [(String, Rational)] -> Box -> Form -> IO Property
retentionProperty label point box form = do
  result <- simplexPruneAfterSimplify sampleMPBall box form
  pure $ counterexample (label P.++ " pruned a feasible point") (pointIsRetained point result)

assertPointRetained :: [(String, Rational)] -> Maybe LinearPruneResult -> Expectation
assertPointRetained point result =
  pointIsRetained point result `shouldBe` True

pointIsRetained :: [(String, Rational)] -> Maybe LinearPruneResult -> Bool
pointIsRetained _ Nothing = True
pointIsRetained _ (Just LinearPruneResult {maybeRemainingBox = Nothing}) = False
pointIsRetained point (Just LinearPruneResult {maybeRemainingBox = Just remainingBox}) =
  P.all retained point
  where
    retained (var, value) =
      case Map.lookup var remainingBox.box_.varDomains of
        Nothing -> False
        Just domain ->
          let (lower, upper) = MP.endpoints domain
           in rational lower P.<= value P.&& value P.<= rational upper
