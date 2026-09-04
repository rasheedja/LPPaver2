module LPPaver2.LinearPrune
  ( extractCIEorDIE,
    IEFormType (..),
    linearPrune,
    linearPruneWithEvalValues,
    LinearPruneResult (..),
    LinearRelaxation (..),
    CanLineariseEval (..),
    linearRelaxation,
    tightenBoxByBounds,
  )
where

import AERN2.MP (HasPrecision (..), mpBallP)
import AERN2.MP qualified as MP
import BranchAndPrune.BranchAndPrune qualified as BP
import Data.Map qualified as Map
import Data.Set qualified as Set
import GHC.Records
import LPPaver2.Linearisation
  ( CanLineariseEval (..),
    LinearRelaxation (..),
    linearRelaxation,
  )
import LPPaver2.RealConstraints (ExprHash, ExprStore, Var)
import LPPaver2.RealConstraints.Boxes
import LPPaver2.RealConstraints.Form
import MixedTypesNumPrelude
import Prelude qualified as P

-- |
--
-- @
--       +------------------------+
--       |         other          |
--       | +-------------+        |
--       | |  DIE        |        |
--       | |      +------+------+ |
--       | |      |  IE  |      | |
--       | +------+------+      | |
--       |        |        CIE  | |
--       |        +-------------+ |
--       +------------------------+
-- @
data IEFormType
  = IE -- single inequality
  | CIE -- conjunction of inequalities
  | DIE -- disjunction of inequalities
  deriving (P.Eq, P.Show)

extractCIEorDIE :: Form -> P.Maybe (Form, IEFormType)
extractCIEorDIE form0 = extractH form0.root
  where
    extractH :: FormHash -> P.Maybe (Form, IEFormType)
    extractH formH =
      let form = form0 {root = formH}
       in case lookupFormNode form0 formH of
            FormComp {comp} ->
              case comp of
                CompLe -> Just (form, IE) -- could be part of a conjunction or disjunction
                CompLeq -> Just (form, IE) -- ditto
                CompEq -> Just (form, CIE) -- a == b  ~  (a <= b) && (a >= b)
                CompNeq -> Just (form, DIE) -- a != b  ~  (a < b) || (a > b)
            FormBinary {bconn, f1, f2} ->
              let f1Info = extractH f1
                  f2Info = extractH f2
               in case bconn of
                    ConnAnd ->
                      case (f1Info, f2Info) of
                        (Just (f1IE, t1), Just (f2IE, t2))
                          | t1 P./= DIE P.&& t2 P./= DIE -> Just (f1IE && f2IE, CIE) -- both compatible with CIE
                          | t1 P./= DIE -> Just (f1IE, CIE) -- only f1 compatible with CIE
                          | t2 P./= DIE -> Just (f2IE, CIE) -- only f2 compatible with CIE
                          | otherwise -> Nothing -- not a conjuction with inequalities
                        (Just (_, CIE), Nothing) -> f1Info
                        (Just (f1IE, IE), Nothing) -> Just (f1IE, CIE) -- conjuction, no longer IE
                        (Nothing, Just (_, CIE)) -> f2Info
                        (Nothing, Just (f2IE, IE)) -> Just (f2IE, CIE) -- conjuction, no longer IE
                        _ -> Nothing
                    ConnOr ->
                      case (f1Info, f2Info) of
                        (Just (f1IE, t1), Just (f2IE, t2))
                          | t1 P./= CIE P.&& t2 P./= CIE -> Just (f1IE || f2IE, DIE) -- both compatible with DIE
                          | t1 P./= CIE -> Just (f1IE, DIE) -- only f1 compatible with DIE
                          | t2 P./= CIE -> Just (f2IE, DIE) -- only f2 compatible with DIE
                          | otherwise -> Nothing -- not a disjunction with inequalities
                        (Just (_, DIE), Nothing) -> f1Info
                        (Just (f1IE, IE), Nothing) -> Just (f1IE, DIE) -- disjunction, no longer IE
                        (Nothing, Just (_, DIE)) -> f2Info
                        (Nothing, Just (f2IE, IE)) -> Just (f2IE, DIE) -- disjunction, no longer IE
                        _ -> Nothing
                    _ -> Nothing
            _ -> Nothing

data LinearPruneResult = LinearPruneResult
  { maybeRemainingBox :: Maybe Box,
    removedRegionTruth :: Bool
  }

linearPrune :: BP.Problem Form Box -> Maybe LinearPruneResult
linearPrune problem = linearPruneWithEvalValues problem (Map.empty :: Map.Map ExprHash MP.MPBall)

linearPruneWithEvalValues ::
  (CanLineariseEval r) =>
  BP.Problem Form Box ->
  Map.Map ExprHash r ->
  Maybe LinearPruneResult
linearPruneWithEvalValues BP.Problem {scope, constraint} exprValues =
  let maybeIEInfo = extractCIEorDIE constraint
   in case maybeIEInfo of
        Just (cieForm, CIE) -> linearPruneCIE scope exprValues (extractIEsFromCIE cieForm)
        Just (ieForm, IE) -> linearPruneCIE scope exprValues [ieForm] -- TODO: try both CIE and DIE and use the better result
        -- TODO: implement linear pruning for disjunctions of inequalities
        _ -> Nothing -- not a form suitable for linear pruning

extractIEsFromCIE :: Form -> [Form]
extractIEsFromCIE form0 = aux form0.root
  where
    aux formH =
      case lookupFormNode form0 formH of
        FormComp {} -> [form0 {root = formH}]
        FormBinary {bconn = ConnAnd, f1, f2} -> aux f1 ++ aux f2
        _ -> error "extractIEsFromCIE: not a CIE form"

data BoundExtraction
  = Infeasible
  | Bounds [(Var, (Maybe Rational, Maybe Rational))]

data TightenResult
  = TightenInfeasible
  | TightenNoImprovement
  | TightenImproved Box

linearPruneCIE :: (CanLineariseEval r) => Box -> Map.Map ExprHash r -> [Form] -> Maybe LinearPruneResult
linearPruneCIE scope exprValues ies
  | any isInfeasible extractionResults =
      Just
        LinearPruneResult
          { maybeRemainingBox = Nothing,
            removedRegionTruth = False
          }
  | otherwise =
      case tightenBoxByBoundsChecked scope varBoundsFromInequalities of
        TightenInfeasible ->
          Just
            LinearPruneResult
              { maybeRemainingBox = Nothing,
                removedRegionTruth = False
              }
        TightenNoImprovement -> Nothing
        TightenImproved newBox -> Just (makeResult newBox)
  where
    extractionResults = P.map extractVarBound ies
    varBoundsFromInequalities = P.concatMap boundsFromResult extractionResults

    isInfeasible :: BoundExtraction -> Bool
    isInfeasible Infeasible = True
    isInfeasible _ = False

    boundsFromResult :: BoundExtraction -> [(Var, (Maybe Rational, Maybe Rational))]
    boundsFromResult (Bounds bounds) = bounds
    boundsFromResult Infeasible = []

    extractVarBound :: Form -> BoundExtraction
    extractVarBound form =
      case lookupFormNode form form.root of
        FormComp {comp, e1, e2} ->
          case comp of
            CompLe -> boundsFromLessOrEqual form.nodesE e1 e2
            CompLeq -> boundsFromLessOrEqual form.nodesE e1 e2
            CompEq -> mergeExtractions (boundsFromLessOrEqual form.nodesE e1 e2) (boundsFromLessOrEqual form.nodesE e2 e1)
            CompNeq -> Bounds []
        _ -> Bounds [] -- not a comparison, shouldn't happen since we only call this on IEs
    boundsFromLessOrEqual :: ExprStore -> ExprHash -> ExprHash -> BoundExtraction
    boundsFromLessOrEqual exprNodes e1 e2 =
      case linearRelaxation exprNodes exprValues e1 e2 of
        Just relaxation -> boundsFromRelaxation relaxation
        Nothing -> Bounds []

    mergeExtractions :: BoundExtraction -> BoundExtraction -> BoundExtraction
    mergeExtractions Infeasible _ = Infeasible
    mergeExtractions _ Infeasible = Infeasible
    mergeExtractions (Bounds b1) (Bounds b2) = Bounds (b1 P.++ b2)

    boundsFromRelaxation :: LinearRelaxation -> BoundExtraction
    boundsFromRelaxation relaxation =
      case activeVars of
        []
          | relaxation.rhs < rational 0 -> Infeasible
          | otherwise -> Bounds []
        [(var, coeff)]
          | coeff > 0 -> Bounds [(var, (Nothing, Just bound))]
          | coeff < 0 -> Bounds [(var, (Just bound, Nothing))]
          | otherwise -> Bounds []
          where
            bound = relaxation.rhs P./ coeff
        _ -> Bounds []
      where
        activeVars = [(var, coeff) | (var, coeff) <- Map.toList relaxation.coefficients, coeff /= 0]

    makeResult newBox =
      LinearPruneResult
        { maybeRemainingBox = Just newBox,
          removedRegionTruth = False
        }

-- | Intersect a box with variable bounds, returning the tightened box only
-- when at least one positive-width domain shrinks by more than ten percent.
tightenBoxByBounds :: Box -> [(Var, (Maybe Rational, Maybe Rational))] -> Maybe Box
tightenBoxByBounds scope bounds =
  case tightenBoxByBoundsChecked scope bounds of
    TightenImproved box -> Just box
    TightenInfeasible -> Nothing
    TightenNoImprovement -> Nothing

tightenBoxByBoundsChecked :: Box -> [(Var, (Maybe Rational, Maybe Rational))] -> TightenResult
tightenBoxByBoundsChecked scope bounds
  | hasEmptyDomain = TightenInfeasible
  | hasSignificantImprovement =
      TightenImproved
        $ boxWithHash
          Box_
            { varDomains = tightenedVarDomains,
              volumeVars = scope.box_.volumeVars,
              splitOrder = scope.box_.splitOrder,
              except = Nothing
            }
  | otherwise = TightenNoImprovement
  where
    varDomains = scope.box_.varDomains
    allTightenedVarDomains = foldl applyBound varDomains bounds
    -- Parameter domains participate in contradiction detection but are never
    -- changed in the result because only volume variables may be pruned.
    tightenedVarDomains =
      Map.mapWithKey
        (\var newDomain ->
           if var `Set.member` scope.box_.volumeVars
             then newDomain
             else varDomains Map.! var
        )
        allTightenedVarDomains

    hasEmptyDomain = P.any domainIsEmpty (Map.toList varDomains)
    domainIsEmpty (var, ball) =
      let (lower, upper) = tightenedEndpoints var ball
       in lower > upper

    tightenedEndpoints var ball =
      P.foldl applyEndpointBound (rational lower0, rational upper0) relevantBounds
      where
        (lower0, upper0) = MP.endpoints ball
        relevantBounds = [bound | (boundVar, bound) <- bounds, boundVar == var]

        applyEndpointBound (lower, upper) (maybeLower, maybeUpper) =
          ( applyLower lower maybeLower,
            applyUpper upper maybeUpper
          )

        applyLower lower (Just bound) = P.max lower bound
        applyLower lower Nothing = lower
        applyUpper upper (Just bound) = P.min upper bound
        applyUpper upper Nothing = upper

    applyBound varDoms (var, (maybeLower, maybeUpper)) = applyUpper $ applyLower varDoms
      where
        applyLower =
          case maybeLower of
            Just lower -> Map.update (Just . tightenLower lower) var
            Nothing -> P.id
        applyUpper =
          case maybeUpper of
            Just upper -> Map.update (Just . tightenUpper upper) var
            Nothing -> P.id

    tightenLower lower ball = mpBallP (getPrecision ball) lower `max` ball
    tightenUpper upper ball = mpBallP (getPrecision ball) upper `min` ball

    hasSignificantImprovement =
      P.any significantImprovement
        $ Map.elems
        $ Map.intersectionWith (,) varDomains tightenedVarDomains

    significantImprovement (oldDomain, newDomain) =
      -- Keep the percentage calculation explicit, guarding the only possible
      -- zero denominator: a fixed domain has radius zero.
      oldRadius > rational 0
        P.&& relativeImprovement > rational 1 P./ rational 10
      where
        oldRadius = rational $ MP.radius oldDomain
        newRadius = rational $ MP.radius newDomain
        relativeImprovement = (oldRadius P.- newRadius) P./ oldRadius
