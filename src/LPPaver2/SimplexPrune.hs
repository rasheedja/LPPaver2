module LPPaver2.SimplexPrune
  ( simplexPrune,
    simplexPruneWithEvalValues,
    CanProvideSimplexRelaxations (..),
  )
where

import AERN2.MP (HasPrecision (..), mpBallP)
import AERN2.MP qualified as MP
import AERN2.MP.Affine (MPAffine)
import Control.Monad.IO.Unlift (MonadIO)
import Control.Monad.Logger (runNoLoggingT)
import Data.Map qualified as Map
import Data.Set qualified as Set
import GHC.Records (HasField (getField))
import LPPaver2.LinearPrune
  ( LinearPruneResult (..),
    tightenBoxByBounds,
  )
import LPPaver2.Linearisation
  ( CanLineariseEval,
    LinearRelaxation (..),
    linearRelaxation,
  )
import LPPaver2.RealConstraints.Boxes
  ( Box (box_),
    Box_ (varDomains, volumeVars),
    boxWithHash,
  )
import LPPaver2.RealConstraints.Eval (evalExpr)
import LPPaver2.RealConstraints.EvalArithmetic.MPBall ()
import LPPaver2.RealConstraints.Expr
  ( ExprHash,
    ExprStore,
    Var,
  )
import LPPaver2.RealConstraints.Expr qualified as Expr
import LPPaver2.RealConstraints.Form
  ( BinaryComp (CompEq, CompLe, CompLeq, CompNeq),
    BinaryConn (ConnAnd),
    Form (nodesE, root),
    FormF (FormBinary, FormComp, bconn, comp, e1, e2, f1, f2),
    FormHash,
    formVariables,
    lookupFormNode,
  )
import LPPaver2.RealConstraints.IntervalAD
  ( IntervalAD (partials),
    evalIntervalAD,
    mpBallBounds,
    subtractIntervalAD,
  )
import Linear.Simplex.Solver.TwoPhase qualified as Simplex
import Linear.Simplex.Types qualified as ST
import MixedTypesNumPrelude
import Prelude qualified as P

class (CanLineariseEval r) => CanProvideSimplexRelaxations r where
  nonlinearSimplexRelaxations ::
    Box ->
    ExprStore ->
    Map.Map ExprHash r ->
    ExprHash ->
    ExprHash ->
    [LinearRelaxation]

instance CanProvideSimplexRelaxations MPAffine where
  nonlinearSimplexRelaxations _ _ _ _ _ = []

instance CanProvideSimplexRelaxations MP.MPBall where
  nonlinearSimplexRelaxations scope exprNodes exprValues e1 e2 =
    intervalCornerRelaxations scope exprNodes exprValues e1 e2

simplexRelaxations ::
  (CanProvideSimplexRelaxations r) =>
  Box ->
  ExprStore ->
  Map.Map ExprHash r ->
  ExprHash ->
  ExprHash ->
  [LinearRelaxation]
simplexRelaxations scope exprNodes exprValues e1 e2 =
  case linearRelaxation exprNodes exprValues e1 e2 of
    Just relaxation -> [relaxation]
    Nothing -> nonlinearSimplexRelaxations scope exprNodes exprValues e1 e2

intervalCornerRelaxations :: Box -> ExprStore -> Map.Map ExprHash MP.MPBall -> ExprHash -> ExprHash -> [LinearRelaxation]
intervalCornerRelaxations scope exprNodes exprValues e1 e2 =
  case derivativeValue of
    Just derivative ->
      case (evalAtCorner leftDomains, evalAtCorner rightDomains) of
        (Just valueAtLeft, Just valueAtRight) ->
          [ leftRelaxation valueAtLeft derivative.partials,
            rightRelaxation valueAtRight derivative.partials
          ]
        _ -> []
    Nothing -> []
  where
    domains = scope.box_.varDomains
    leftDomains = Map.map pointAtLower domains
    rightDomains = Map.map pointAtUpper domains

    -- The comparison e1 <= e2 is represented as t = e2 - e1 >= 0.
    derivativeValue = subtractIntervalAD <$> evalIntervalAD exprNodes exprValues domains e2 <*> evalIntervalAD exprNodes exprValues domains e1

    evalAtCorner cornerDomains = do
      value1 <- Map.lookup e1 values
      value2 <- Map.lookup e2 values
      pure $ value2 P.- value1
      where
        cornerBox = boxWithHash $ scope.box_ {varDomains = cornerDomains}
        valuesAfterE1 =
          evalExpr
            sampleBall
            cornerBox
            Expr.Expr {Expr.nodes = exprNodes, Expr.root = e1}
            Map.empty
        values =
          evalExpr
            sampleBall
            cornerBox
            Expr.Expr {Expr.nodes = exprNodes, Expr.root = e2}
            valuesAfterE1

    sampleBall = mpBallP precision (rational 0)
    precision = case Map.elems domains of
      domain : _ -> getPrecision domain
      [] -> MP.defaultPrecision

    domainBounds =
      [ (var, rational lower, rational upper)
        | (var, domain) <- Map.toList domains,
          let (lower, upper) = MP.endpoints domain
      ]

    derivativeBounds partialMap var =
      case Map.lookup var partialMap of
        Just derivative -> mpBallBounds derivative
        Nothing -> (rational 0, rational 0)

    upperBound = P.snd . mpBallBounds

    leftRelaxation valueAtLeft partialMap =
      LinearRelaxation
        { coefficients = Map.fromList [(var, P.negate derivativeUpper) | (var, _, _) <- domainBounds, let (_, derivativeUpper) = derivativeBounds partialMap var],
          rhs = upperBound valueAtLeft P.- sum [derivativeUpper P.* lower | (var, lower, _) <- domainBounds, let (_, derivativeUpper) = derivativeBounds partialMap var]
        }

    rightRelaxation valueAtRight partialMap =
      LinearRelaxation
        { coefficients = Map.fromList [(var, P.negate derivativeLower) | (var, _, _) <- domainBounds, let (derivativeLower, _) = derivativeBounds partialMap var],
          rhs = upperBound valueAtRight P.- sum [derivativeLower P.* upper | (var, _, upper) <- domainBounds, let (derivativeLower, _) = derivativeBounds partialMap var]
        }

    pointAtLower = MP.endpointLAsInterval
    pointAtUpper = MP.endpointRAsInterval

-- | Create a mapping from LPPaver2 variable names (String) to simplex variable IDs (Int).
createVarMapping :: Set.Set Var -> Box -> (Map.Map Var Int, Map.Map Int Var)
createVarMapping relevantVars box =
  let vars = P.filter (`Set.member` relevantVars) $ Map.keys box.box_.varDomains
      varToInt = Map.fromList (P.zip vars (P.map int [1 ..]))
      intToVar = Map.fromList (P.zip (P.map int [1 ..]) vars)
   in (varToInt, intToVar)

data ConstraintExtraction
  = Infeasible
  | Constraints [ST.PolyConstraint]

-- | Convert a sound linear relaxation to a simplex constraint.
relaxationToSimplexConstraint ::
  Map.Map Var Int ->
  LinearRelaxation ->
  ConstraintExtraction
relaxationToSimplexConstraint varToInt relaxation =
  let activeVars = [(var, coeff) | (var, coeff) <- Map.toList relaxation.coefficients, coeff /= 0]

      -- Check if any active variable is missing from our simplex mapping
      hasUnknownVars = P.any (\(var, _) -> not (Map.member var varToInt)) activeVars
   in if P.null activeVars
        then
          if relaxation.rhs < rational 0
            then Infeasible
            else Constraints []
        else
          if hasUnknownVars
            then Constraints [] -- Safely discard the constraint; we can't bound the unknown variables
            else
              let lhsMap =
                    Map.fromList
                      [ (intVar, coeff)
                        | (var, coeff) <- Map.toList relaxation.coefficients,
                          coeff /= 0,
                          Just intVar <- [Map.lookup var varToInt]
                      ]
               in if Map.null lhsMap
                    then Constraints []
                    else Constraints [ST.LEQ {lhs = lhsMap, rhs = relaxation.rhs}]

-- | Extract simplex constraints from a conjunction of inequalities.
extractSimplexConstraints ::
  (CanProvideSimplexRelaxations r) =>
  Box ->
  ExprStore ->
  Map.Map ExprHash r ->
  Map.Map Var Int ->
  Form ->
  ConstraintExtraction
extractSimplexConstraints scope exprNodes exprValues varToInt form0 =
  extractFromRoot form0.root
  where
    extractFromRoot :: FormHash -> ConstraintExtraction
    extractFromRoot fH =
      case lookupFormNode form0 fH of
        FormComp {comp, e1, e2} ->
          case comp of
            CompLe -> constraintFromLE e1 e2
            CompLeq -> constraintFromLE e1 e2
            CompEq ->
              -- a == b → a ≤ b ∧ b ≤ a
              mergeConstraints (constraintFromLE e1 e2) (constraintFromLE e2 e1)
            CompNeq -> Constraints [] -- can't express as LP constraint
        FormBinary {bconn = ConnAnd, f1, f2} ->
          mergeConstraints (extractFromRoot f1) (extractFromRoot f2)
        _ -> Constraints []

    mergeConstraints Infeasible _ = Infeasible
    mergeConstraints _ Infeasible = Infeasible
    mergeConstraints (Constraints c1) (Constraints c2) = Constraints (c1 P.++ c2)

    constraintFromLE :: ExprHash -> ExprHash -> ConstraintExtraction
    constraintFromLE e1H e2H =
      P.foldl
        mergeConstraints
        (Constraints [])
        (relaxationToSimplexConstraint varToInt <$> simplexRelaxations scope exprNodes exprValues e1H e2H)

-- | Create simplex variable domain constraints from box bounds.
boxToVarDomains :: Map.Map Var Int -> Box -> ST.VarDomainMap
boxToVarDomains varToInt box =
  ST.VarDomainMap
    $ Map.fromList
      [ (intVar, ST.boundedRange lo hi)
        | (var, ball) <- Map.toList box.box_.varDomains,
          Just intVar <- [Map.lookup var varToInt],
          let (l, u) = MP.endpoints ball,
          let lo = rational l,
          let hi = rational u
      ]

simplexPruneWithEvalValues ::
  (MonadIO m, CanProvideSimplexRelaxations r) =>
  Box ->
  Form ->
  Map.Map ExprHash r ->
  m (Maybe LinearPruneResult)
simplexPruneWithEvalValues = simplexPrune

-- | Use the simplex method to tighten a box given linear constraints.
-- For each variable, maximize and minimize subject to all constraints.
-- Returns a tighter box if any improvement is found.
simplexPrune ::
  (MonadIO m, CanProvideSimplexRelaxations r) =>
  Box ->
  Form ->
  Map.Map ExprHash r ->
  m (Maybe LinearPruneResult)
simplexPrune scope simplifiedForm exprValues = do
  let (varToInt, intToVar) = createVarMapping (formVariables simplifiedForm) scope
  let constraintExtraction = extractSimplexConstraints scope simplifiedForm.nodesE exprValues varToInt simplifiedForm
  let varDomains = boxToVarDomains varToInt scope
  case constraintExtraction of
    Infeasible ->
      pure $ Just LinearPruneResult {maybeRemainingBox = Nothing, removedRegionTruth = False}
    Constraints constraints ->
      -- If we extracted no linear constraints beyond box bounds, simplex won't help
      if P.null constraints
        then pure Nothing
        else do
          -- For each variable, minimize and maximize
          let vars = Map.toList varToInt
              objectives =
                P.concatMap
                  ( \(var, intVar) ->
                      if var `Set.member` scope.box_.volumeVars
                        then
                          [ ST.Min {objective = Map.singleton intVar (rational 1)},
                            ST.Max {objective = Map.singleton intVar (rational 1)}
                          ]
                        else []
                  )
                  vars

          result <- runNoLoggingT $ Simplex.twoPhaseSimplex varDomains objectives constraints

          case result.feasibleSystem of
            Nothing ->
              -- Infeasible means the constraint conjunction is unsatisfiable on this box
              pure
                $ Just
                  LinearPruneResult
                    { maybeRemainingBox = Nothing,
                      removedRegionTruth = False -- the constraint is false on the entire box
                    }
            Just _ -> do
              -- Extract tightened bounds from objective results
              let objResults = result.objectiveResults
              let newBounds = extractBoundsFromResults intToVar objResults
              let tightenedBox = tightenBoxByBounds scope (Map.toList newBounds)
              case tightenedBox of
                Just box -> pure $ Just LinearPruneResult {maybeRemainingBox = Just box, removedRegionTruth = False}
                Nothing -> pure Nothing

-- | Extract bounds using the objective carried by each simplex result.
extractBoundsFromResults ::
  Map.Map Int Var ->
  [ST.ObjectiveResult] ->
  Map.Map Var (Maybe Rational, Maybe Rational)
extractBoundsFromResults intToVar objResults = P.foldl processResult Map.empty objResults
  where
    processResult acc objResult =
      case (objectiveTarget objResult.objectiveFunction, objResult.outcome) of
        (Just (intVar, isLowerBound, coefficient), ST.Optimal {varValMap}) ->
          case Map.lookup intVar intToVar of
            Just var ->
              let objectiveValue = Simplex.computeObjective objResult.objectiveFunction varValMap
                  value = objectiveValue P./ coefficient
                  current = Map.findWithDefault (Nothing, Nothing) var acc
               in if isLowerBound
                    then Map.insert var (Just value, P.snd current) acc
                    else Map.insert var (P.fst current, Just value) acc
            Nothing -> acc
        _ -> acc

    objectiveTarget objectiveFunction =
      case objectiveFunction of
        ST.Min {objective} -> targetFromTerms True objective
        ST.Max {objective} -> targetFromTerms False objective

    targetFromTerms isMin objective =
      case Map.toList objective of
        [(intVar, coefficient)]
          | coefficient /= rational 0 ->
              Just
                ( intVar,
                  (isMin P.&& coefficient > rational 0)
                    P.|| (P.not isMin P.&& coefficient < rational 0),
                  coefficient
                )
        _ -> Nothing
