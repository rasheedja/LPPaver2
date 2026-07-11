module LPPaver2.LinearPrune
  ( extractCIEorDIE,
    IEFormType (..),
    linearPrune,
    linearPruneWithEvalBounds,
    LinearPruneResult (..),
  )
where

import AERN2.MP (HasPrecision (..), mpBallP)
import AERN2.MP qualified as MP
import BranchAndPrune.BranchAndPrune qualified as BP
import Data.Map qualified as Map
import GHC.Records
import LPPaver2.RealConstraints (BinaryOp (..), ExprF (..), ExprHash, ExprStore, UnaryOp (..), Var)
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
linearPrune problem = linearPruneWithBounds problem Map.empty

linearPruneWithEvalBounds ::
  (ConvertibleExactly r MP.MPBall) =>
  BP.Problem Form Box ->
  Map.Map ExprHash r ->
  Maybe LinearPruneResult
linearPruneWithEvalBounds problem exprValues =
  linearPruneWithBounds problem (exprValueBounds exprValues)

type ExprBounds = Map.Map ExprHash (Rational, Rational)

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

linearPruneWithBounds :: BP.Problem Form Box -> ExprBounds -> Maybe LinearPruneResult
linearPruneWithBounds BP.Problem {scope, constraint} exprBounds =
  let maybeIEInfo = extractCIEorDIE constraint
   in case maybeIEInfo of
        Just (cieForm, CIE) -> linearPruneCIE scope exprBounds (extractIEsFromCIE cieForm)
        Just (ieForm, IE) -> linearPruneCIE scope exprBounds [ieForm] -- TODO: try both CIE and DIE and use the better result
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

data Decomposition = Decomposition
  { coefficients :: Map.Map Var Rational,
    constant :: Rational,
    nlLower :: Rational,
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

linearVar :: Var -> Decomposition
linearVar var = emptyDecomp {coefficients = Map.singleton var (rational 1)}

linearConst :: Rational -> Decomposition
linearConst value = emptyDecomp {constant = value}

nonLinearTerm :: Rational -> Rational -> Decomposition
nonLinearTerm lo hi = emptyDecomp {nlLower = lo, nlUpper = hi}

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
scaleDecomp scale d
  | scale >= 0 =
      Decomposition
        { coefficients = Map.map (P.* scale) d.coefficients,
          constant = d.constant P.* scale,
          nlLower = d.nlLower P.* scale,
          nlUpper = d.nlUpper P.* scale
        }
  | otherwise =
      Decomposition
        { coefficients = Map.map (P.* scale) d.coefficients,
          constant = d.constant P.* scale,
          nlLower = d.nlUpper P.* scale,
          nlUpper = d.nlLower P.* scale
        }

decomposeExpr :: ExprStore -> ExprBounds -> ExprHash -> Maybe Decomposition
decomposeExpr exprNodes exprBounds = go
  where
    go eH =
      case Map.lookup eH exprNodes of
        Nothing -> fallback eH
        Just node ->
          case node of
            ExprVar {var} -> Just $ linearVar var
            ExprLit {lit} -> Just $ linearConst lit
            ExprUnary {unop = OpNeg, e1} ->
              case go e1 of
                Just d -> Just $ negateDecomp d
                Nothing -> Nothing
            ExprUnary {} -> fallback eH
            ExprBinary {binop = OpPlus, e1, e2} ->
              case (go e1, go e2) of
                (Just d1, Just d2) -> Just $ addDecomps d1 d2
                _ -> Nothing
            ExprBinary {binop = OpMinus, e1, e2} ->
              case (go e1, go e2) of
                (Just d1, Just d2) -> Just $ subDecomps d1 d2
                _ -> Nothing
            ExprBinary {binop = OpTimes, e1, e2} ->
              case (Map.lookup e1 exprNodes, Map.lookup e2 exprNodes) of
                (Just (ExprLit {lit}), _) ->
                  case go e2 of
                    Just d -> Just $ scaleDecomp lit d
                    Nothing -> Nothing
                (_, Just (ExprLit {lit})) ->
                  case go e1 of
                    Just d -> Just $ scaleDecomp lit d
                    Nothing -> Nothing
                _ -> fallback eH
            ExprBinary {binop = OpDivide, e1, e2} ->
              case Map.lookup e2 exprNodes of
                Just (ExprLit {lit}) | lit /= 0 ->
                  case go e1 of
                    Just d -> Just $ scaleDecomp (P.recip lit) d
                    Nothing -> Nothing
                _ -> fallback eH

    fallback eH =
      case Map.lookup eH exprBounds of
        Just (lo, hi) -> Just $ nonLinearTerm lo hi
        Nothing -> Nothing

linearPruneCIE :: Box -> ExprBounds -> [Form] -> Maybe LinearPruneResult
linearPruneCIE scope exprBounds ies
  | isImprovement = Just result
  | otherwise = Nothing
  where
    varBoundsFromInequalities = P.concatMap extractVarBound ies

    extractVarBound form =
      case lookupFormNode form form.root of
        FormComp {comp, e1, e2} ->
          case comp of
            CompLe -> boundsFromLessOrEqual form.nodesE e1 e2
            CompLeq -> boundsFromLessOrEqual form.nodesE e1 e2
            CompEq -> boundsFromLessOrEqual form.nodesE e1 e2 P.++ boundsFromLessOrEqual form.nodesE e2 e1
            CompNeq -> []
        _ -> [] -- not a comparison, shouldn't happen since we only call this on IEs

    boundsFromLessOrEqual exprNodes e1 e2 =
      case (decomposeExpr exprNodes exprBounds e1, decomposeExpr exprNodes exprBounds e2) of
        (Just d1, Just d2) -> boundsFromDiff (subDecomps d1 d2)
        _ -> []

    boundsFromDiff diff =
      case activeVars of
        [(var, coeff)]
          | coeff > 0 -> [(var, (Nothing, Just bound))]
          | coeff < 0 -> [(var, (Just bound, Nothing))]
          | otherwise -> []
          where
            bound = (P.negate diff.constant P.- diff.nlLower) P./ coeff
        _ -> []
      where
        activeVars = [(var, coeff) | (var, coeff) <- Map.toList diff.coefficients, coeff /= 0]

    varDomains = scope.box_.varDomains
    varDomainsWithInequalities = foldl applyBound varDomains varBoundsFromInequalities
      where
        applyBound varDoms (var, (maybeQL, maybeQU)) =
          applyUpper $ applyLower varDoms
          where
            applyLower =
              case maybeQL of
                Just qL -> Map.update (updateLower qL) var
                Nothing -> P.id
            applyUpper =
              case maybeQU of
                Just qU -> Map.update (updateUpper qU) var
                Nothing -> P.id
        updateLower qL ball =
          Just res
          where
            res = qLMB `max` ball
            qLMB = mpBallP (getPrecision ball) qL
        updateUpper qU ball =
          Just res
          where
            res = qUMB `min` ball
            qUMB = mpBallP (getPrecision ball) qU
    isImprovement =
      res
      where
        res = P.any (> 0.1) $ Map.elems improvements
        improvements = Map.intersectionWith measureImprovement varDomains varDomainsWithInequalities
        measureImprovement ballOld ballNew =
          let rOld = MP.radius ballOld
              rNew = MP.radius ballNew
           in (rational rOld - rational rNew) / rational rOld
    newBox =
      boxWithHash
        Box_
          { varDomains = varDomainsWithInequalities,
            splitOrder = scope.box_.splitOrder,
            except = Nothing
          }
    result =
      LinearPruneResult
        { maybeRemainingBox = Just newBox,
          removedRegionTruth = False
        }
