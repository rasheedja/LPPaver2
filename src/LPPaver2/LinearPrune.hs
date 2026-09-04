module LPPaver2.LinearPrune
  ( extractCIEorDIE,
    IEFormType (..),
    linearPrune,
    LinearPruneResult (..),
  )
where

import AERN2.MP (HasPrecision (..), mpBallP)
import AERN2.MP qualified as MP
import BranchAndPrune.BranchAndPrune qualified as BP
import Data.Map qualified as Map
import Data.Set qualified as Set
import Debug.Trace (trace)
import GHC.Records
import LPPaver2.RealConstraints (ExprF (..))
import LPPaver2.RealConstraints.Boxes
import LPPaver2.RealConstraints.Form
import MixedTypesNumPrelude
import Text.Printf (printf)
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
linearPrune BP.Problem {scope, constraint} =
  let maybeIEInfo = extractCIEorDIE constraint
   in case maybeIEInfo of
        Just (cieForm, CIE) -> linearPruneCIE scope (extractIEsFromCIE cieForm)
        Just (ieForm, IE) -> linearPruneCIE scope [ieForm] -- TODO: try both CIE and DIE and use the better result
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

linearPruneCIE :: Box -> [Form] -> Maybe LinearPruneResult
linearPruneCIE scope ies
  | hasEmptyDomain =
      Just
        LinearPruneResult
          { maybeRemainingBox = Nothing,
            removedRegionTruth = False
          }
  -- trace (printf "linearPruneCIE: scope = %s, ies = %s" (show scope) (show ies)) $
  -- trace (printf "  varDomains = %s" (show varDomains)) $
  -- trace (printf "  varDomainsWithInequalities = %s" (show varDomainsWithInequalities)) $
  -- trace (printf "  isImprovement = %s" (show isImprovement)) $
  | isImprovement = Just result
  | otherwise = Nothing
  where
    varBoundsFromInequalities = P.concatMap extractVarBound ies
      where
        extractVarBound form =
          case lookupFormNode form form.root of
            FormComp {comp, e1, e2} ->
              case (lookupFormExprNode form e1, comp, lookupFormExprNode form e2) of
                (ExprVar var, CompLe, ExprLit q) -> [(var, (Nothing, Just q))]
                (ExprVar var, CompLeq, ExprLit q) -> [(var, (Nothing, Just q))]
                (ExprLit q, CompLe, ExprVar var) -> [(var, (Just q, Nothing))]
                (ExprLit q, CompLeq, ExprVar var) -> [(var, (Just q, Nothing))]
                _ -> []
            _ -> [] -- not a comparison, shouldn't happen since we only call this on IEs
    volumeVarDomains = -- pick domains of volume variables only, since parameter variables cannot be pruned
      Map.filterWithKey (\k _ -> k `Set.member` scope.box_.volumeVars) scope.box_.varDomains
    hasEmptyDomain = P.any domainIsEmpty (Map.toList volumeVarDomains)
    domainIsEmpty (var, ball) =
      let (lower, upper) = P.foldl applyEndpointBound (rational lower0, rational upper0) relevantBounds
          (lower0, upper0) = MP.endpoints ball
          relevantBounds = [bound | (boundVar, bound) <- varBoundsFromInequalities, boundVar == var]
       in lower > upper
    applyEndpointBound (lower, upper) (maybeLower, maybeUpper) =
      ( maybe lower (P.max lower) maybeLower,
        maybe upper (P.min upper) maybeUpper
      )
    varDomainsWithInequalities = foldl applyBound volumeVarDomains varBoundsFromInequalities
      where
        applyBound varDoms (var, (Just qL, _)) =
          Map.update (updateLower qL) var varDoms
        applyBound varDoms (var, (_, Just qU)) =
          Map.update (updateUpper qU) var varDoms
        applyBound varDoms _ = varDoms -- shouldn't happen since we only call this on IEs
        updateLower qL ball =
          -- trace
          --   ( printf
          --       "updateLower: qL = %s, prec = %s, qLB = %s, ball = %s, result = %s"
          --       (show qL)
          --       (show (getPrecision ball))
          --       (show qLMB)
          --       (show ball)
          --       (show res)
          --   )
          Just res
          where
            res = qLMB `max` ball
            qLMB = mpBallP (getPrecision ball) qL
        updateUpper qU ball =
          -- trace
          --   ( printf
          --       "updateUpper: qU = %s, prec = %s, qUB = %s, ball = %s, result = %s"
          --       (show qU)
          --       (show (getPrecision ball))
          --       (show qUMB)
          --       (show ball)
          --       (show res)
          --   )
          Just res
          where
            res = qUMB `min` ball
            qUMB = mpBallP (getPrecision ball) qU
    isImprovement =
      -- trace
      --   ( printf
      --       "linearPruneCIE:\n varDomains = %s\n varDomainsWithInequalities = %s\n improvements = %s\n isImprovement = %s"
      --       (show varDomains)
      --       (show varDomainsWithInequalities)
      --       (show improvements)
      --       (show res)
      --   )
      res
      where
        res = P.any (> 0.1) $ Map.elems improvements
        improvements = Map.intersectionWith measureImprovement volumeVarDomains varDomainsWithInequalities
        measureImprovement ballOld ballNew =
          let rOld = MP.radius ballOld
              rNew = MP.radius ballNew
           in (rational rOld - rational rNew) / rational rOld
    newBox =
      boxWithHash
        Box_
          { varDomains = varDomainsWithInequalities,
            splitOrder = scope.box_.splitOrder,
            volumeVars = scope.box_.volumeVars,
            except = Nothing
          }
    result =
      LinearPruneResult
        { maybeRemainingBox = Just newBox,
          removedRegionTruth = False
        }
