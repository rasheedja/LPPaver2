module LPPaver2.LinearPruneSpec (spec) where

import BranchAndPrune.BranchAndPrune qualified as BP
import MixedTypesNumPrelude
import LPPaver2.LinearPrune
import LPPaver2.RealConstraints.Boxes (mkBox)
import LPPaver2.RealConstraints.Form
import LPPaver2.RealConstraints.Expr
import Test.Hspec

x :: Expr
x = exprVar "x"

y :: Expr
y = exprVar "y"

lit1 :: Expr
lit1 = exprLit 1.0

lit2 :: Expr
lit2 = exprLit 2.0

spec :: Spec
spec = describe "extractCIEorDIE" $ do
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

  it "detects contradictory extracted bounds" $ do
    let form = (x <= exprLit 0.0) && (exprLit 1.0 <= x)
        box = mkBox [("x", (0.0, 1.0))]

    assertInfeasible $ linearPrune BP.Problem {scope = box, constraint = form}

  it "detects extracted bounds outside the original box" $ do
    let form = x <= exprLit (-1.0)
        box = mkBox [("x", (0.0, 1.0))]

    assertInfeasible $ linearPrune BP.Problem {scope = box, constraint = form}

assertInfeasible :: Maybe LinearPruneResult -> Expectation
assertInfeasible (Just LinearPruneResult {maybeRemainingBox = Nothing, removedRegionTruth = False}) = pure ()
assertInfeasible _ = expectationFailure "expected an infeasible pruning result"
