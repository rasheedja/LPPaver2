module LPPaver2.RealConstraints.EvalSpec (spec) where

import AERN2.Kleenean
import AERN2.MP (MPBall, mpBall)
import AERN2.MP qualified as MP (endpointsAsIntervals)
import Data.Map qualified as Map
import GHC.Records (HasField (getField))
import LPPaver2.RealConstraints.Boxes
import LPPaver2.RealConstraints.Eval
import LPPaver2.RealConstraints.EvalArithmetic.MPBall ()
import LPPaver2.RealConstraints.Expr
import LPPaver2.RealConstraints.Form
import MixedTypesNumPrelude
import Test.Hspec

simplifyEvalFormMB :: Box -> Form -> SimplifyFormResultR MPBall
simplifyEvalFormMB = simplifyEvalFormR (mpBall (0 :: Integer))

simplifyOverUnitX :: Form -> SimplifyFormResultR MPBall
simplifyOverUnitX = simplifyEvalFormMB (mkBox [("x", (0.0, 1.0))])

x :: Expr
x = exprVar "x"

lit1 :: Expr
lit1 = exprLit 1.0

lit2 :: Expr
lit2 = exprLit 2.0

litHalf :: Expr
litHalf = exprLit 0.5

spec :: Spec
spec = describe "simplifyEvalForm" $ do
  it "evaluates trivial constants correctly (True)" $ do
    let result = simplifyOverUnitX formTrue
        resultForm = result.evaluatedForm.form
        formValues = result.evaluatedForm.formValues
    resultForm `shouldBe` formTrue -- unchanged
    (result.oldToNew Map.! formTrue.root) `shouldBe` formTrue.root -- identity mapping
    -- evaluated as true
    (formValues Map.! formTrue.root) `shouldBe` CertainTrue

  it "evaluates trivial constants correctly (False)" $ do
    let result = simplifyOverUnitX formFalse
        resultForm = result.evaluatedForm.form
        formValues = result.evaluatedForm.formValues
    resultForm `shouldBe` formFalse -- unchanged
    (result.oldToNew Map.! formFalse.root) `shouldBe` formFalse.root -- identity mapping
    -- evaluated as false
    (formValues Map.! formFalse.root) `shouldBe` CertainFalse

  it "simplifies simple comparisons correctly (1 <= 2)" $ do
    let form = lit1 <= lit2
        result = simplifyOverUnitX form
        resultForm = result.evaluatedForm.form
    -- simplified to True
    resultForm `shouldBe` formTrue

  it "simplifies simple comparisons correctly (2 <= 1)" $ do
    let form = lit2 <= lit1
        result = simplifyOverUnitX form
        resultForm = result.evaluatedForm.form
    -- simplified to False
    resultForm `shouldBe` formFalse

  it "simplifies expressions with variables correctly (x <= 2 over [0,1])" $ do
    let form = x <= lit2
        result = simplifyOverUnitX form
        resultForm = result.evaluatedForm.form
    -- simplified to True
    resultForm `shouldBe` formTrue

  it "simplifies expressions with variables correctly (x >= 2 over [0,1])" $ do
    let form = lit2 <= x
        result = simplifyOverUnitX form
        resultForm = result.evaluatedForm.form
    -- simplified to False
    resultForm `shouldBe` formFalse

  it "leaves uncertain forms unresolved (x <= 0.5 over [0,1])" $ do
    let form = x <= litHalf
        result = simplifyOverUnitX form
        resultForm = result.evaluatedForm.form
    -- unchanged, since it's not decided
    resultForm `shouldBe` form

  it "simplifies Boolean connectives (True and False)" $ do
    let form = formTrue && formFalse
        result = simplifyOverUnitX form
        resultForm = result.evaluatedForm.form
    -- simplified to False
    resultForm `shouldBe` formFalse

  it "simplifies Boolean connectives (not False)" $ do
    let form = not formFalse
        result = simplifyOverUnitX form
        resultForm = result.evaluatedForm.form
    -- simplified to True
    resultForm `shouldBe` formTrue

  it "simplifies Boolean connectives (True or (x <= 0.5))" $ do
    let form = formTrue || x <= litHalf
        result = simplifyOverUnitX form
        resultForm = result.evaluatedForm.form
    -- simplified to True
    resultForm `shouldBe` formTrue

  it "simplifies if-then-else when condition is True" $ do
    let formThenBranch = x <= litHalf
        form = formIfThenElse (x <= lit2) formThenBranch formFalse
        result = simplifyOverUnitX form
        resultForm = result.evaluatedForm.form
    -- simplified to the "then" branch
    resultForm `shouldBe` formThenBranch

  it "simplifies if-then-else when condition is False" $ do
    let formElseBranch = x <= litHalf
        form = formIfThenElse (x > lit2) formFalse formElseBranch
        result = simplifyOverUnitX form
        resultForm = result.evaluatedForm.form
    -- simplified to the "else" branch
    resultForm `shouldBe` formElseBranch

  it "simplifies if-then-else when branches are decided (if C then True else False)" $ do
    let formC = x <= litHalf
        form = formIfThenElse formC formTrue formFalse
        result = simplifyOverUnitX form
        resultForm = result.evaluatedForm.form
    -- reduced to the undecided condition only since the branches are True and False
    resultForm `shouldBe` formC

  it "simplifies if-then-else when branches are decided (if C then False else True)" $ do
    let formC = x <= litHalf
        form = formIfThenElse formC formFalse formTrue
        result = simplifyOverUnitX form
        resultForm = result.evaluatedForm.form
    -- reduced to the undecided condition only since the branches are True and False
    resultForm `shouldBe` not formC

  it "returns values for sub-expressions (x - 1 <= 2 over [0,1])" $ do
    let form = x - lit1 <= lit2
        result = simplifyOverUnitX form
        exprValues = result.evaluatedForm.exprValues
    MP.endpointsAsIntervals (exprValues Map.! lit1.root) `shouldBe` (mpBall 1, mpBall 1)
    MP.endpointsAsIntervals (exprValues Map.! lit2.root) `shouldBe` (mpBall 2, mpBall 2)
    MP.endpointsAsIntervals (exprValues Map.! x.root) `shouldBe` (mpBall 0, mpBall 1)
    MP.endpointsAsIntervals (exprValues Map.! (x - lit1).root) `shouldBe` (mpBall (-1), mpBall 0)

  it "returns values for sub-formulas (True and False)" $ do
    let form = formTrue && formFalse
        result = simplifyOverUnitX form
        formValues = result.evaluatedForm.formValues
    (formValues Map.! form.root) `shouldBe` CertainFalse -- evaluated as false
    (formValues Map.! formTrue.root) `shouldBe` CertainTrue -- contains values for sub-formulas
    (formValues Map.! formFalse.root) `shouldBe` CertainFalse

  it "returns simplification mapping for sub-formulas (True and False)" $ do
    let form = formTrue && formFalse
        result = simplifyOverUnitX form
    (result.oldToNew Map.! form.root) `shouldBe` formFalse.root -- mapping original conjunction to False (simplified)
    (result.oldToNew Map.! formTrue.root) `shouldBe` formTrue.root -- True cannot be simplified
    (result.oldToNew Map.! formFalse.root) `shouldBe` formFalse.root
