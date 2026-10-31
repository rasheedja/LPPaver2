module LPPaver2.LinearPruneSpec (spec) where

import AERN2.MP qualified as MP
import AERN2.MP.Affine (MPAffine (..))
import BranchAndPrune.BranchAndPrune qualified as BP
import Data.Map qualified as Map
import GHC.Records (HasField (getField))
import LPPaver2.LinearPrune
import LPPaver2.PruneSpecSupport
import LPPaver2.RealConstraints.Boxes (Box (..), Box_ (..), addParamValuesToBox, mkBox)
import LPPaver2.RealConstraints.Eval
  ( CanEval,
    HasKleeneanComparison,
  )
import LPPaver2.RealConstraints.EvalArithmetic.AffArith ()
import LPPaver2.RealConstraints.EvalArithmetic.MPBall ()
import LPPaver2.RealConstraints.Expr
import LPPaver2.RealConstraints.Form
import MixedTypesNumPrelude
import Test.Hspec
import Prelude qualified as P

lit1 :: Expr
lit1 = exprLit 1.0

lit2 :: Expr
lit2 = exprLit 2.0

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

  describe "linearPruneWithEvalValues" $ do
    it "uses current-box affine sources across multiple nonlinear comparisons" $ do
      let form = (sin (x - x) + y <= exprLit 0.5) && (sin (y - y) + x <= exprLit 0.25)
          boxes =
            [ mkBox [("x", (0.0, 1.0)), ("y", (0.0, 1.0))],
              mkBox [("x", (-2.0, 2.0)), ("y", (-4.0, 4.0))]
            ]

      P.mapM_
        (\box -> do
           let result = linearPruneAfterSimplify sampleMPAffine box form
           assertPrunedUpperBoundWithin aaBoundTolerance "x" 0.25 result
           assertPrunedUpperBoundWithin aaBoundTolerance "y" 0.5 result
        )
        boxes

    it "rejects ambiguous affine sources for every comparison while retaining exact relaxation" $ do
      let left1 = sin x + y
          left2 = sin y + x
          right1 = exprLit 0.5
          right2 = exprLit 0.25
          form = (left1 <= right1) && (left2 <= right2)
          box = mkBox [("x", (0.0, 1.0)), ("y", (0.0, 1.0))]
          (_, exprValues) = simplifiedFormAndValues sampleMPAffine box form
          -- Make two different source variables claim the same noise symbol.
          ambiguousValues = Map.adjust (\affine -> affine {errTerms = (exprValues Map.! x.root).errTerms}) y.root exprValues
          relax = linearRelaxation form.nodesE ambiguousValues

      case (relax left1.root right1.root, relax left2.root right2.root) of
        (Nothing, Nothing) -> pure ()
        _ -> expectationFailure "expected both affine relaxations to reject ambiguous sources"
      case relax x.root right1.root of
        Just relaxation -> do
          relaxation.coefficients `shouldBe` Map.singleton "x" (rational 1)
          relaxation.rhs `shouldBe` rational 0.5
        Nothing -> expectationFailure "expected exact relaxation to remain available"

    it "uses an affine relaxation when available" $ do
      let form = sin (x - x) + y <= exprLit 0.5
          box = mkBox [("x", (0.0, 1.0)), ("y", (0.0, 1.0))]

      let iaResult = linearPruneAfterSimplify sampleMPBall box form
          aaResult = linearPruneAfterSimplify sampleMPAffine box form

      expectNoPruning iaResult
      assertPrunedLowerBoundWithin aaBoundTolerance "y" 0.0 aaResult
      assertPrunedUpperBoundWithin aaBoundTolerance "y" 0.5 aaResult

    it "uses an affine relaxation to tighten lower bounds" $ do
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

    it "detects contradictory extracted bounds" $ do
      let form = (x <= exprLit 0.0) && (exprLit 1.0 <= x)
          box = mkBox [("x", (0.0, 1.0))]

      assertInfeasible $ linearPrune BP.Problem {scope = box, constraint = form}

    it "detects extracted bounds outside the original box" $ do
      let form = x <= exprLit (-1.0)
          box = mkBox [("x", (0.0, 1.0))]

      assertInfeasible $ linearPrune BP.Problem {scope = box, constraint = form}

    it "detects contradictory parameter bounds while tightening volume variables" $ do
      let baseBox = mkBox [("x", (0.0, 10.0))]
          box = addParamValuesToBox (Map.fromList [("p", 1.0)]) baseBox
          form = (x <= exprLit 5.0) && (exprVar "p" <= exprLit 0.0)

      assertInfeasible $ linearPrune BP.Problem {scope = box, constraint = form}

    it "preserves fixed parameter domains while tightening volume variables" $ do
      let baseBox = mkBox [("x", (0.0, 10.0))]
          box = addParamValuesToBox (Map.fromList [("p", 1.0)]) baseBox
          form = x <= exprLit 5.0

      case linearPrune BP.Problem {scope = box, constraint = form} of
        Just LinearPruneResult {maybeRemainingBox = Just remainingBox} -> do
          let domains = remainingBox.box_.varDomains
          MP.endpoints (domains Map.! "x") `shouldSatisfy` (\(lower, upper) -> rational lower == 0 && rational upper == 5)
          MP.endpoints (domains Map.! "p") `shouldSatisfy` (\(lower, upper) -> rational lower == 1 && rational upper == 1)
          remainingBox.box_.volumeVars `shouldBe` box.box_.volumeVars
        _ -> expectationFailure "expected a tightened box"

linearPruneAfterSimplify ::
  (CanEval r, HasKleeneanComparison r, CanLineariseEval r) =>
  r ->
  Box ->
  Form ->
  Maybe LinearPruneResult
linearPruneAfterSimplify sampleR box form =
  linearPruneWithEvalValues BP.Problem {scope = box, constraint = simplifiedForm} exprValues
  where
    (simplifiedForm, exprValues) = simplifiedFormAndValues sampleR box form
