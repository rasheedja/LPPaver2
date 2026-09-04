{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module LPPaver2.RealConstraints.Eval
  ( CanGetVarDomain (..),
    CanEval,
    evalExpr,
    evalExprWith,
    HasKleeneanComparison,
    simplifyEvalFormR,
    SimplifyFormResultR (..),
    EvaluatedFormR (..),
    OldToNew,
  )
where

import AERN2.Kleenean
import Data.Map qualified as Map
import GHC.Records (HasField (getField))
import LPPaver2.RealConstraints.Boxes (Box)
import LPPaver2.RealConstraints.Expr
  ( BinaryOp (..),
    Expr (..),
    ExprF (..),
    ExprHash,
    UnaryOp (..),
    Var,
  )
import LPPaver2.RealConstraints.Form
import LPPaver2.RealConstraints.Form qualified as Form
import MixedTypesNumPrelude

class CanGetVarDomain r where
  -- | the first parameter is a value sample to help type inference
  getVarDomain :: r -> Box -> Var -> r

type CanEval r =
  ( CanGetVarDomain r,
    Ring r,
    CanDivSameType r,
    HasRationalsWithSample r,
    CanSqrtSameType r,
    CanSinCosSameType r
  )

evalExpr ::
  (CanEval r) =>
  r ->
  Box ->
  Expr ->
  Map.Map ExprHash r ->
  Map.Map ExprHash r
evalExpr sampleR box expr =
  evalExprWith evaluateNode expr
  where
    evaluateNode _ ExprVar {var} = getVarDomain sampleR box var
    evaluateNode _ ExprLit {lit} = convertExactlyWithSample sampleR lit
    evaluateNode _ ExprUnary {unop, e1} = evalUnop unop e1
    evaluateNode _ ExprBinary {binop, e1, e2} = evalBinop binop e1 e2

-- | Evaluate an expression DAG using a caller-supplied node algebra.
--
-- Child hashes are replaced with their already-computed values before the
-- algebra is called.  Values are memoised by 'ExprHash', including values
-- supplied in the initial map, so shared subexpressions are evaluated once.
evalExprWith ::
  (ExprHash -> ExprF r -> r) ->
  Expr ->
  Map.Map ExprHash r ->
  Map.Map ExprHash r
evalExprWith evaluateNode expr = evalNode expr.root
  where
    nodes = expr.nodes
    evalNode h valuesSoFar =
      case Map.lookup h valuesSoFar of
        -- if this node's value is already in the value dictionary, return the dictionary as is
        Just _ -> valuesSoFar
        Nothing ->
          -- lookup the node details
          case Map.lookup h nodes of
            Nothing -> error "evalExprWith: a hash is missing from expr.nodes"
            Just node ->
              -- evaluate the node and it to the value dictionary
              case node of
                ExprVar {var} -> Map.insert h (evaluateNode h ExprVar {var}) valuesSoFar
                ExprLit {lit} -> Map.insert h (evaluateNode h ExprLit {lit}) valuesSoFar
                ExprUnary {unop, e1} ->
                  let valuesAfterE1 = evalNode e1 valuesSoFar
                      e1Value = valuesAfterE1 Map.! e1
                   in Map.insert h (evaluateNode h ExprUnary {unop, e1 = e1Value}) valuesAfterE1
                ExprBinary {binop, e1, e2} ->
                  let valuesAfterE1 = evalNode e1 valuesSoFar
                      e1Value = valuesAfterE1 Map.! e1
                      valuesAfterE2 = evalNode e2 valuesAfterE1
                      e2Value = valuesAfterE2 Map.! e2
                   in Map.insert h (evaluateNode h ExprBinary {binop, e1 = e1Value, e2 = e2Value}) valuesAfterE2

evalUnop ::
  ( CanNegSameType r,
    CanSqrtSameType r,
    CanSinCosSameType r
  ) =>
  UnaryOp ->
  r ->
  r
evalUnop OpNeg val = -val
evalUnop OpSqrt val = sqrt val
evalUnop OpSin val = sin val
evalUnop OpCos val = cos val

evalBinop ::
  ( CanAddSameType r,
    CanSubSameType r,
    CanMulSameType r,
    CanDivSameType r
  ) =>
  BinaryOp ->
  r ->
  r ->
  r
evalBinop OpPlus v1 v2 = v1 + v2
evalBinop OpMinus v1 v2 = v1 - v2
evalBinop OpTimes v1 v2 = v1 * v2
evalBinop OpDivide v1 v2 = v1 / v2

type HasKleeneanComparison r =
  ( HasOrder r r,
    OrderCompareType r r ~ Kleenean,
    HasEq r r,
    EqCompareType r r ~ Kleenean
  )

-- |
--  A formula with the values of all its sub-expresions, the intermediate values
--  of evaluating the truth value of the formula over some set.
--
--  As the formulas are aggresively simplified while evaluating, their
--  truth value can be tested using `getFormDecision` which simply compares
--  the formula to FormTrue and FormFalse,
data EvaluatedFormR r = EvaluatedFormR
  { form :: Form,
    exprValues :: Map.Map ExprHash r,
    formValues :: Map.Map FormHash Kleenean
  }

instance Show (EvaluatedFormR r) where
  show _ = "EvaluatedFormR..."

data SimplifyFormResultR r = SimplifyFormResultR
  { evaluatedForm :: EvaluatedFormR r,
    oldToNew :: OldToNew
  }

-- utility for convenient extraction of all four result elements at once
flattenResult :: SimplifyFormResultR r -> (Form, Map.Map ExprHash r, Map.Map FormHash Kleenean, OldToNew)
flattenResult result =
  ( result.evaluatedForm.form,
    result.evaluatedForm.exprValues,
    result.evaluatedForm.formValues,
    result.oldToNew
  )

type OldToNew = Map.Map FormHash FormHash

buildResult ::
  OldToNew -> FormHash -> EvaluatedFormR r -> SimplifyFormResultR r
buildResult oldToNew oldH evaluatedForm =
  SimplifyFormResultR
    { evaluatedForm,
      oldToNew = Map.insert oldH evaluatedForm.form.root oldToNew
    }

resultWithH :: SimplifyFormResultR r -> FormHash -> SimplifyFormResultR r
resultWithH result h =
  resultWithForm result (result.evaluatedForm.form {Form.root = h})

resultWithForm :: SimplifyFormResultR r -> Form -> SimplifyFormResultR r
resultWithForm result f =
  result {evaluatedForm = result.evaluatedForm {form = f}}

simplifyEvalFormR ::
  (CanEval r, HasKleeneanComparison r) =>
  r ->
  Box ->
  Form ->
  SimplifyFormResultR r
simplifyEvalFormR (sapleR :: r) box formInit =
  simplify
    SimplifyFormResultR
      { evaluatedForm =
          EvaluatedFormR
            { form = formInit,
              exprValues = Map.empty,
              formValues = Map.empty
            },
        oldToNew = Map.empty
      }
  where
    -- Traverse the formula nodes, evaluating over the box to true/false/don't know and
    -- replacing decided sub-formulas with True / False nodes.
    -- While traversing, we accummulate dictionaries of expressions.

    -- As expression structure does not change, we can use the initial expression nodes dictionary
    -- in all sub-formulas.  Prepare a shortcut for evaluating an expression given by its hash:
    evalEH eH = evalExpr sapleR box (Expr {nodes = formInit.nodesE, root = eH})

    simplifyH prevResult h = simplify (resultWithForm prevResult (formInit {Form.root = h}))

    simplify :: (_) => SimplifyFormResultR r -> SimplifyFormResultR r
    simplify result0 =
      simplifyNodeReusingPrev form0.root
      where
        (form0, _, _, oldToNew0) = flattenResult result0

        simplifyNodeReusingPrev h =
          case Map.lookup h oldToNew0 of
            -- if we have simplified this sub-formula previously, reuse the previous result
            Just newH ->
              resultWithH result0 newH
            Nothing ->
              simplifyNode h

        simplifyNode h =
          -- branch by the type of node
          case lookupFormNode form0 h of
            FormTrue ->
              simplifyConst result0 h formTrue
            FormFalse ->
              simplifyConst result0 h formFalse
            FormComp binComp e1H e2H ->
              simplifyComp evalEH result0 h binComp e1H e2H
            FormUnary op f1H ->
              simplifyUnary simplifyH result0 h op f1H
            FormBinary op f1H f2H ->
              simplifyBinary simplifyH result0 h op f1H f2H
            FormIfThenElse fcH ftH ffH ->
              simplifyIf simplifyH result0 h fcH ftH ffH

simplifyConst :: SimplifyFormResultR r -> FormHash -> Form -> SimplifyFormResultR r
simplifyConst result0 h cForm =
  let (_, exprValues0, formValues0, oldToNew0) = flattenResult result0
      formValues = Map.insert h (getFormDecision cForm) formValues0
   in buildResult oldToNew0 h (EvaluatedFormR {form = cForm, exprValues = exprValues0, formValues})

simplifyComp ::
  (HasKleeneanComparison r) =>
  (ExprHash -> Map.Map ExprHash r -> Map.Map ExprHash r) ->
  SimplifyFormResultR r ->
  FormHash ->
  BinaryComp ->
  ExprHash ->
  ExprHash ->
  SimplifyFormResultR r
simplifyComp evalEH result0 h binComp e1H e2H =
  let (form0, exprValues0, formValues0, oldToNew0) = flattenResult result0
      -- evaluate the two expressions
      exprValues1 = evalEH e1H exprValues0
      e1Value = exprValues1 Map.! e1H
      exprValues12 = evalEH e2H exprValues1
      e2Value = exprValues12 Map.! e2H
      -- evaluate the comparison
      comparison = case binComp of
        CompLe -> e1Value < e2Value
        CompLeq -> e1Value <= e2Value
        CompEq -> e1Value == e2Value
        CompNeq -> e1Value /= e2Value
      -- update the form values with the comparison result
      formValues = Map.insert h comparison formValues0
      -- build the result with the simplified form (True/False if decided, or the original form if not)
      buildR f =
        buildResult oldToNew0 h (EvaluatedFormR {form = f, exprValues = exprValues12, formValues})
   in case comparison of
        CertainTrue -> buildR formTrue
        CertainFalse -> buildR formFalse
        _ -> buildR (form0 {Form.root = h})

simplifyUnary ::
  (SimplifyFormResultR r -> FormHash -> SimplifyFormResultR r) ->
  SimplifyFormResultR r ->
  FormHash ->
  UnaryConn -> -- negation is the only unary connective, can ignore this parameter
  FormHash ->
  SimplifyFormResultR r
simplifyUnary simplifyH result0 h ConnNeg f1H =
  let -- recursively simplify the sub-formula
      result1 = simplifyH result0 f1H
      (simplifiedF1, exprValues1, formValues1, oldToNew1) = flattenResult result1
      -- update the form values with the negation of the sub-formula's value
      formValues = Map.insert h (negate (formValues1 Map.! f1H)) formValues1
      -- check whether the formula is decided
      decision1 = getFormDecision simplifiedF1
      -- build the result with the simplified form (True/False if decided,
      --  or the negation of the simplified sub-formula if not)
      buildR f =
        buildResult oldToNew1 h (EvaluatedFormR {form = f, exprValues = exprValues1, formValues})
   in case decision1 of
        CertainTrue -> buildR formFalse
        CertainFalse -> buildR formTrue
        _ -> buildR (not simplifiedF1)

simplifyBinary ::
  (SimplifyFormResultR r -> FormHash -> SimplifyFormResultR r) ->
  SimplifyFormResultR r ->
  FormHash ->
  BinaryConn ->
  FormHash ->
  FormHash ->
  SimplifyFormResultR r
simplifyBinary simplifyH result0 h binaryConn f1H f2H =
  let -- recursively simplify the two sub-formulas
      result1 = simplifyH result0 f1H
      (simplifiedF1, _, _, _) = flattenResult result1
      result2 = simplifyH result1 f2H
      (simplifiedF2, exprValues12, formValues12, oldToNew12) = flattenResult result2
      -- simplify also the negation of the first sub-formula, as it is needed for implication simplification
      resultNegF1 = simplifyUnary simplifyH result1 h ConnNeg f1H
      (simplifiedNegF1, _, formValues1Neg, _) = flattenResult resultNegF1

      -- check if the two sub-formulas are decided
      decision1 = getFormDecision simplifiedF1
      decision2 = getFormDecision simplifiedF2
      -- helper for building the result with the simplified form
      buildR decision f =
        let formValues = Map.insert h decision (Map.union formValues1Neg formValues12)
         in buildResult oldToNew12 h (EvaluatedFormR {form = f, exprValues = exprValues12, formValues})
   in case binaryConn of
        ConnAnd ->
          case (decision1, decision2) of
            -- if one of the sub-formulas is false, the whole formula is false
            (CertainFalse, _) -> buildR CertainFalse formFalse
            (_, CertainFalse) -> buildR CertainFalse formFalse
            -- if one of the sub-formulas is true, the whole formula has the same value as the other sub-formula
            (CertainTrue, _) -> buildR decision2 simplifiedF2
            (_, CertainTrue) -> buildR decision1 simplifiedF1
            -- retain the binary connective if neither sub-formula is decided, use simplified sub-formulas
            _ -> buildR TrueOrFalse $ simplifiedF1 && simplifiedF2
        ConnOr ->
          case (decision1, decision2) of
            -- if one of the sub-formulas is true, the whole formula is true
            (CertainTrue, _) -> buildR CertainTrue formTrue
            (_, CertainTrue) -> buildR CertainTrue formTrue
            -- if one of the sub-formulas is false, the whole formula has the same value as the other sub-formula
            (CertainFalse, _) -> buildR decision2 simplifiedF2
            (_, CertainFalse) -> buildR decision1 simplifiedF1
            -- retain the binary connective if neither sub-formula is decided, use simplified sub-formulas
            _ -> buildR TrueOrFalse $ simplifiedF1 || simplifiedF2
        ConnImpl ->
          case (decision1, decision2) of
            -- "False -> A": always true (false implies anything)
            (CertainFalse, _) -> buildR CertainTrue formTrue
            -- "A -> True": always true
            (_, CertainTrue) -> buildR CertainTrue formTrue
            -- "True -> A": true premise can be dropped
            (CertainTrue, _) -> buildR decision2 simplifiedF2
            -- "A -> False": equivalent to "not A"
            (_, CertainFalse) -> buildR (not decision1) simplifiedNegF1
            -- if neither sub-formula is decided, retain the implication, use the simplified sub-formulas
            _ -> buildR TrueOrFalse $ formImpl simplifiedF1 simplifiedF2

simplifyIf ::
  (SimplifyFormResultR r -> FormHash -> SimplifyFormResultR r) ->
  SimplifyFormResultR r ->
  FormHash ->
  FormHash ->
  FormHash ->
  FormHash ->
  SimplifyFormResultR r
simplifyIf simplifyH result0 h fcH ftH ffH =
  let -- recursively simplify the condition, then the two branches
      resultC = simplifyH result0 fcH
      (simplifiedC, _, _, _) = flattenResult resultC
      resultT = simplifyH resultC ftH
      (simplifiedT, _, _, _) = flattenResult resultT
      resultF = simplifyH resultT ffH
      (simplifiedF, exprValuesCTF, formValuesCTF, oldToNewCTF) = flattenResult resultF
      -- simplify also the negation of the condition, since one of the rules needs it
      resultCNeg = simplifyUnary simplifyH result0 h ConnNeg fcH
      (simplifiedCNeg, _, formValuesCNeg, _) = flattenResult resultCNeg
      -- check which of the three sub-formulas are decided
      decisionC = getFormDecision simplifiedC
      decisionT = getFormDecision simplifiedT
      decisionF = getFormDecision simplifiedF
      -- helper for building the result with the simplified form
      buildR decision f =
        let formValues = Map.insert h decision (formValuesCTF `Map.union` formValuesCNeg)
         in buildResult oldToNewCTF h (EvaluatedFormR {form = f, exprValues = exprValuesCTF, formValues})
   in case (decisionC, decisionT, decisionF) of
        -- "if True then A else B" is equivalent to A
        (CertainTrue, _, _) -> buildR decisionT simplifiedT
        -- "if False then A else B" is equivalent to B
        (CertainFalse, _, _) -> buildR decisionF simplifiedF
        -- "if C then True else True" is always true
        (_, CertainTrue, CertainTrue) -> buildR CertainTrue formTrue
        -- "if C then False else False" is always false
        (_, CertainFalse, CertainFalse) -> buildR CertainFalse formFalse
        -- "if C then True else False" is equivalent to C
        (_, CertainTrue, CertainFalse) -> buildR TrueOrFalse simplifiedC
        -- "if C then False else True" is equivalent to not C
        (_, CertainFalse, CertainTrue) -> buildR TrueOrFalse simplifiedCNeg
        -- "if C then True else B" is equivalent to "C or B"
        (_, CertainTrue, _) -> buildR TrueOrFalse $ simplifiedC || simplifiedF
        -- "if C then False else B" is equivalent to "not C and B"
        (_, CertainFalse, _) -> buildR TrueOrFalse $ simplifiedCNeg && simplifiedF
        -- "if C then A else True" is equivalent to "not C or A"
        (_, _, CertainTrue) -> buildR TrueOrFalse $ simplifiedCNeg || simplifiedT
        -- "if C then A else False" is equivalent to "C and A"
        (_, _, CertainFalse) -> buildR TrueOrFalse $ simplifiedC && simplifiedT
        -- if none of the sub-formulas is decided, retain the if-then-else structure with the simplified sub-formulas
        _ -> buildR TrueOrFalse $ formIfThenElse simplifiedC simplifiedT simplifiedF
