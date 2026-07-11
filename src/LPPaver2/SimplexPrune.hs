module LPPaver2.SimplexPrune
  ( simplexPrune,
    simplexPruneWithEvalBounds,
  )
where

import MixedTypesNumPrelude
import Prelude qualified as P

import LPPaver2.RealConstraints.Boxes
    ( boxWithHash,
      Box(box_),
      Box_(Box_, varDomains, except, splitOrder) )
import LPPaver2.RealConstraints.Expr
    ( BinaryOp(OpTimes, OpPlus, OpMinus, OpDivide),
      ExprF(ExprVar, ExprUnary, ExprBinary, ExprLit, lit, var, unop,
            binop, e1, e2),
      ExprHash,
      ExprStore,
      UnaryOp(OpNeg, OpSin, OpSqrt, OpCos),
      Var )
import LPPaver2.RealConstraints.Form
    ( lookupFormNode,
      BinaryComp(CompNeq, CompLe, CompLeq, CompEq),
      BinaryConn(ConnAnd),
      Form(nodesE, root),
      FormF(FormComp, FormBinary, f2, comp, e1, e2, bconn, f1),
      FormHash )
import LPPaver2.LinearPrune (LinearPruneResult(..))

import Control.Monad.IO.Unlift (MonadIO)
import Control.Monad.Logger (runNoLoggingT)
import Data.Map qualified as Map
import GHC.Records ( HasField(getField) )

import AERN2.MP (HasPrecision (..), mpBallP)
import AERN2.MP qualified as MP
import Linear.Simplex.Solver.TwoPhase qualified as Simplex
import Linear.Simplex.Types qualified as ST

-- | A linear decomposition of an expression.
--
-- Represents: (sum of coefficients * variables) + constant + nonLinearRemainder
-- where nonLinearRemainder is bounded by [nlLower, nlUpper].
data Decomposition = Decomposition -- TODO: why do we need this?
  { coefficients :: Map.Map Var Rational, -- this is like the simplex system constraints
    constant :: Rational, -- this would be the rhs
    -- TODO: actual interval type?
    -- FIXME: looks dodgy
    nlLower :: Rational, -- check how the non-linear remainder is being used
    nlUpper :: Rational
  }

emptyDecomp :: Decomposition
emptyDecomp =
  Decomposition
    { coefficients = Map.empty,
      constant = rational 0,
      nlLower = rational 0,
      nlUpper = rational 0
    }

-- | A purely linear decomposition (no non-linear remainder).
linearVar :: Var -> Decomposition
linearVar v =
  emptyDecomp {coefficients = Map.singleton v (rational 1)}

linearConst :: Rational -> Decomposition
linearConst c = emptyDecomp {constant = c}

-- | A purely non-linear term with given interval bounds.
nonLinearTerm :: Rational -> Rational -> Decomposition
nonLinearTerm lo hi =
  emptyDecomp {nlLower = lo, nlUpper = hi}

negateDecomp :: Decomposition -> Decomposition
negateDecomp d =
  Decomposition
    { coefficients = Map.map P.negate d.coefficients,
      constant = P.negate d.constant,
      nlLower = P.negate d.nlUpper,
      nlUpper = P.negate d.nlLower
    }


addDecomps :: Decomposition -> Decomposition -> Decomposition
addDecomps d1 d2 =
  Decomposition
    { coefficients = Map.unionWith (P.+) d1.coefficients d2.coefficients,
      constant = d1.constant P.+ d2.constant,
      nlLower = d1.nlLower P.+ d2.nlLower,
      nlUpper = d1.nlUpper P.+ d2.nlUpper
    }

subDecomps :: Decomposition -> Decomposition -> Decomposition
subDecomps d1 d2 = addDecomps d1 (negateDecomp d2)

scaleDecomp :: Rational -> Decomposition -> Decomposition
scaleDecomp c d
  | c >= 0 =
      Decomposition
        { coefficients = Map.map (P.* c) d.coefficients,
          constant = d.constant P.* c,
          nlLower = d.nlLower P.* c,
          nlUpper = d.nlUpper P.* c
        }
  | otherwise =
      Decomposition
        { coefficients = Map.map (P.* c) d.coefficients,
          constant = d.constant P.* c,
          nlLower = d.nlUpper P.* c,
          nlUpper = d.nlLower P.* c
        }

-- | Check if a decomposition has any linear variable terms.
hasLinearTerms :: Decomposition -> Bool
hasLinearTerms d = not (Map.null d.coefficients)

-- | Decompose an expression into linear part + non-linear remainder.
-- Uses the expression DAG and interval evaluations for non-linear sub-expressions.
decomposeExpr ::
  ExprStore ->
  Map.Map ExprHash (Rational, Rational) ->
  ExprHash ->
  Decomposition
decomposeExpr exprNodes exprBounds = go
  where
    go :: ExprHash -> Decomposition
    go eH =
      case Map.lookup eH exprNodes of
        Nothing -> fallback eH
        Just node -> case node of
          ExprVar {var} -> linearVar var
          ExprLit {lit} -> linearConst lit
          ExprUnary {unop = OpNeg, e1} ->
            negateDecomp (go e1)
          ExprBinary {binop = OpPlus, e1, e2} ->
            addDecomps (go e1) (go e2)
          ExprBinary {binop = OpMinus, e1, e2} ->
            subDecomps (go e1) (go e2)
          ExprBinary {binop = OpTimes, e1, e2} ->
            case (Map.lookup e1 exprNodes, Map.lookup e2 exprNodes) of
              (Just (ExprLit {lit}), _) -> scaleDecomp lit (go e2)
              (_, Just (ExprLit {lit})) -> scaleDecomp lit (go e1)
              _otherwise -> fallback eH
          ExprBinary {binop = OpDivide, e1, e2} ->
            case Map.lookup e2 exprNodes of
              Just (ExprLit {lit}) | lit /= 0 -> scaleDecomp (P.recip lit) (go e1)
              _otherwise -> fallback eH
          ExprUnary {unop = OpSin} -> fallback eH
          ExprUnary {unop = OpCos} -> fallback eH
          ExprUnary {unop = OpSqrt} -> fallback eH

    -- TODO: better name, it gets bounds for nl expr
    fallback :: ExprHash -> Decomposition
    fallback eH =
      case Map.lookup eH exprBounds of
        Just (lo, hi) -> nonLinearTerm lo hi
        Nothing -> error "shouldn't be here!"
          -- emptyDecomp -- shouldn't happen if evaluation is complete TODO: error out!

-- | Create a mapping from LPPaver2 variable names (String) to simplex variable IDs (Int).
createVarMapping :: Box -> (Map.Map Var Int, Map.Map Int Var)
createVarMapping box =
  let vars = Map.keys box.box_.varDomains
      -- FIXME: this may be used unsoundly!
      varToInt = Map.fromList (P.zip vars (P.map int [1 ..]))
      intToVar = Map.fromList (P.zip (P.map int [1 ..]) vars)
   in (varToInt, intToVar)

data ConstraintExtraction
  = Infeasible
  | Constraints [ST.PolyConstraint]

-- | Convert a linear decomposition constraint (lhs ≤ rhs) to a simplex PolyConstraint.
decompToSimplexConstraint ::
  Map.Map Var Int ->
  Decomposition ->
  Decomposition ->
  ConstraintExtraction
decompToSimplexConstraint varToInt d1 d2 =
  let diff = subDecomps d1 d2
      -- Extract variables that actually have an impact (non-zero coefficient)
      activeVars = [ (var, coeff) | (var, coeff) <- Map.toList diff.coefficients, coeff /= 0 ]

      -- Check if any active variable is missing from our simplex mapping
      hasUnknownVars = P.any (\(var, _) -> not (Map.member var varToInt)) activeVars

   in if P.null activeVars
        then
          if diff.constant P.+ diff.nlLower P.> rational 0
            then Infeasible
            else Constraints []
        else if hasUnknownVars
        then Constraints [] -- Safely discard the constraint; we can't bound the unknown variables
        else
          let lhsMap =
                Map.fromList
                  [(intVar, coeff) |
                     (var, coeff) <- Map.toList diff.coefficients,
                     coeff /= 0,
                     Just intVar <- [Map.lookup var varToInt]]
              rhs = P.negate diff.constant P.+ (diff.nlUpper P.- diff.nlLower)
              -- The constraint is: linear_part + nl_part ≤ 0
              -- → linear_part ≤ -nl_lower (since nl_part ≥ nl_lower)
              -- Actually: diff = d1 - d2, we want d1 ≤ d2
              -- → diff.linear + diff.nl ≤ 0
              -- → diff.linear ≤ -diff.nl
              -- → diff.linear ≤ -diff.nlLower (safe relaxation since -diff.nl ≤ -diff.nlLower)
              rhsCorrect = P.negate diff.nlLower P.- diff.constant -- TODO: looks fishy
           in if Map.null lhsMap
                then Constraints []
                else Constraints [ST.LEQ {lhs = lhsMap, rhs = rhsCorrect}]

-- | Convert a linear decomposition constraint (lhs ≤ rhs) to a simplex PolyConstraint.
-- -- Given: d1 comp d2 where comp is ≤ or <
-- -- We produce: (coeffs1 - coeffs2) · x ≤ (const2 - const1) + (nlUpper2 - nlLower1)
-- decompToSimplexConstraint ::
--   Map.Map Var Int ->
--   Decomposition ->
--   Decomposition ->
--   P.Maybe ST.PolyConstraint
-- decompToSimplexConstraint varToInt d1 d2 =
--   let diff = subDecomps d1 d2
--    in if not (hasLinearTerms diff)
--         then Nothing -- no linear terms to constrain -- TODO: is it safe to drop things?
--         else
--           let lhsMap =
--                 Map.fromList
--                   [ (intVar, coeff)
--                     | (var, coeff) <- Map.toList diff.coefficients,
--                       Just intVar <- [Map.lookup var varToInt],
--                       coeff P./= rat 0
--                   ]
--               rhs = P.negate diff.constant P.+ (diff.nlUpper P.- diff.nlLower)
--               -- The constraint is: linear_part + nl_part ≤ 0
--               -- → linear_part ≤ -nl_lower (since nl_part ≥ nl_lower)
--               -- Actually: diff = d1 - d2, we want d1 ≤ d2
--               -- → diff.linear + diff.nl ≤ 0
--               -- → diff.linear ≤ -diff.nl
--               -- → diff.linear ≤ -diff.nlLower (safe relaxation since -diff.nl ≤ -diff.nlLower)
--               rhsCorrect = P.negate diff.nlLower P.- diff.constant -- TODO: looks fishy
--            in if Map.null lhsMap
--                 then Nothing
--                 else Just $ ST.LEQ {lhs = lhsMap, rhs = rhsCorrect}

-- | Extract simplex constraints from a conjunction of inequalities.
extractSimplexConstraints ::
  ExprStore ->
  Map.Map ExprHash (Rational, Rational) ->
  Map.Map Var Int ->
  Form ->
  ConstraintExtraction
extractSimplexConstraints exprNodes exprBounds varToInt form0 =
  extractFromRoot form0.root
  where
    decompose = decomposeExpr exprNodes exprBounds

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
      let d1 = decompose e1H
          d2 = decompose e2H
       in decompToSimplexConstraint varToInt d1 d2

-- | Create simplex variable domain constraints from box bounds.
boxToVarDomains :: Map.Map Var Int -> Box -> ST.VarDomainMap
boxToVarDomains varToInt box =
  ST.VarDomainMap $
    Map.fromList
      [ (intVar, ST.boundedRange lo hi)
        | (var, ball) <- Map.toList box.box_.varDomains,
          Just intVar <- [Map.lookup var varToInt],
          let (l, u) = MP.endpoints ball,
          let lo = rational l,
          let hi = rational u
      ]

type ExprBounds = Map.Map ExprHash (Rational, Rational)

-- | Convert evaluated expression values into endpoint bounds used for
-- nonlinear fallback terms. The arithmetic type decides how tight these
-- bounds are: MPBall gives interval arithmetic bounds, while MPAffine gives
-- affine-arithmetic bounds before conversion to endpoint intervals.
exprValueBounds ::
  (ConvertibleExactly r MP.MPBall) =>
  Map.Map ExprHash r ->
  ExprBounds
exprValueBounds = Map.map valueBounds
  where
    valueBounds r =
      let ball = convertExactly r :: MP.MPBall
          (lo, hi) = MP.endpoints ball
       in (rational lo, rational hi)

simplexPruneWithEvalBounds ::
  (MonadIO m, ConvertibleExactly r MP.MPBall) =>
  Box ->
  Form ->
  Map.Map ExprHash r ->
  m (Maybe LinearPruneResult)
simplexPruneWithEvalBounds scope simplifiedForm exprValues =
  simplexPrune scope simplifiedForm (exprValueBounds exprValues)

-- | Use the simplex method to tighten a box given linear constraints.
-- For each variable, maximize and minimize subject to all constraints.
-- Returns a tighter box if any improvement is found.
simplexPrune ::
  (MonadIO m) =>
  Box ->
  Form ->
  ExprBounds ->
  m (Maybe LinearPruneResult)
simplexPrune scope simplifiedForm exprBounds = do
  let (varToInt, intToVar) = createVarMapping scope
  let constraintExtraction = extractSimplexConstraints simplifiedForm.nodesE exprBounds varToInt simplifiedForm
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
          let objectives =
                P.concatMap
                  (\(_, intVar) ->
                    [ ST.Min {objective = Map.singleton intVar (rational 1)},
                      ST.Max {objective = Map.singleton intVar (rational 1)}
                    ]
                  )
                  vars

          result <- runNoLoggingT $ Simplex.twoPhaseSimplex varDomains objectives constraints

          case result.feasibleSystem of
            Nothing ->
              -- Infeasible means the constraint conjunction is unsatisfiable on this box
              pure $
                Just
                  LinearPruneResult
                    { maybeRemainingBox = Nothing,
                      removedRegionTruth = False -- the constraint is false on the entire box
                    }
            Just _ -> do
              -- Extract tightened bounds from objective results
              -- FIXME: does it do anything with the objective var
              let objResults = result.objectiveResults
              let newBounds = extractBoundsFromResults intToVar objResults
              let tightenedBox = applyNewBounds scope newBounds
              case tightenedBox of
                Just box -> pure $ Just LinearPruneResult {maybeRemainingBox = Just box, removedRegionTruth = False}
                Nothing -> pure Nothing

-- | Extract min/max bounds for each variable from simplex objective results.
-- Objectives are in pairs: [Min x1, Max x1, Min x2, Max x2, ...]
extractBoundsFromResults ::
  Map.Map Int Var ->
  [ST.ObjectiveResult] ->
  Map.Map Var (Maybe Rational, Maybe Rational)
extractBoundsFromResults intToVar objResults = P.foldl processResult Map.empty (P.zip (P.map int [0 ..]) objResults)
  where
    vars = Map.toList intToVar
    processResult acc (idx, objResult) =
      let varIdx = idx `P.div` int 2
          isMin = P.even idx
       in case vars P.!! varIdx of
            (intVar, var) ->
              case objResult.outcome of
                ST.Optimal {varValMap} ->
                  let val = Map.findWithDefault (rational 0) intVar varValMap
                      current = Map.findWithDefault (Nothing, Nothing) var acc
                   in if isMin
                        then Map.insert var (Just val, P.snd current) acc
                        else Map.insert var (P.fst current, Just val) acc
                ST.Unbounded -> acc

-- | Apply new bounds to a box, returning a tighter box if there's improvement.
applyNewBounds ::
  Box ->
  Map.Map Var (Maybe Rational, Maybe Rational) ->
  Maybe Box
applyNewBounds scope newBounds
  | isImprovement =
      Just $
        boxWithHash
          Box_
            { varDomains = tightenedVarDomains,
              splitOrder = scope.box_.splitOrder,
              except = Nothing
            }
  | otherwise = Nothing
  where
    varDomains = scope.box_.varDomains
    tightenedVarDomains = Map.mapWithKey tightenVar varDomains

    tightenVar var ball =
      case Map.lookup var newBounds of
        Nothing -> ball
        Just (maybeLo, maybeHi) ->
          let ball1 = case maybeLo of
                Just lo ->
                  let loBall = mpBallP (getPrecision ball) lo
                   in loBall `max` ball
                Nothing -> ball
              ball2 = case maybeHi of
                Just hi ->
                  let hiBall = mpBallP (getPrecision ball) hi
                   in hiBall `min` ball1
                Nothing -> ball1
           in ball2

    isImprovement =
      P.any (P.> (1 % 10)) $ Map.elems improvements
      where
        improvements = Map.intersectionWith measureImprovement varDomains tightenedVarDomains
        measureImprovement ballOld ballNew =
          let rOld = MP.radius ballOld
              rNew = MP.radius ballNew
           in (rational rOld P.- rational rNew) P./ rational rOld
