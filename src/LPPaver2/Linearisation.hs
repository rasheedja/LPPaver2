module LPPaver2.Linearisation
  ( CanLineariseEval (..),
    LinearRelaxation (..),
    linearRelaxation,
  )
where

import AERN2.MP qualified as MP
import AERN2.MP.Affine (ErrorTermId, MPAffine (..))
import AERN2.MP.Dyadic (dyadic)
import AERN2.MP.Float (MPFloat)
import Data.Map qualified as Map
import GHC.Records
import LPPaver2.RealConstraints.Eval (evalExprWith)
import LPPaver2.RealConstraints.Expr
  ( BinaryOp (..),
    ExprF (..),
    ExprHash,
    ExprStore,
    UnaryOp (..),
    Var,
  )
import LPPaver2.RealConstraints.Expr qualified as Expr
import MixedTypesNumPrelude
import Prelude qualified as P

data AffineExpression = AffineExpression
  { expressionCoefficients :: Map.Map Var Rational,
    expressionConstant :: Rational
  }

data LinearRelaxation = LinearRelaxation
  { coefficients :: Map.Map Var Rational,
    rhs :: Rational
  }

zeroExpression :: AffineExpression
zeroExpression = AffineExpression {expressionCoefficients = Map.empty, expressionConstant = rational 0}

linearVar :: Var -> AffineExpression
linearVar var = zeroExpression {expressionCoefficients = Map.singleton var (rational 1)}

linearConst :: Rational -> AffineExpression
linearConst value = zeroExpression {expressionConstant = value}

negateExpression :: AffineExpression -> AffineExpression
negateExpression expression =
  AffineExpression
    { expressionCoefficients = Map.map P.negate expression.expressionCoefficients,
      expressionConstant = P.negate expression.expressionConstant
    }

addExpressions :: AffineExpression -> AffineExpression -> AffineExpression
addExpressions expression1 expression2 =
  AffineExpression
    { expressionCoefficients = Map.unionWith (P.+) expression1.expressionCoefficients expression2.expressionCoefficients,
      expressionConstant = expression1.expressionConstant P.+ expression2.expressionConstant
    }

subtractExpressions :: AffineExpression -> AffineExpression -> AffineExpression
subtractExpressions expression1 expression2 = addExpressions expression1 (negateExpression expression2)

scaleExpression :: Rational -> AffineExpression -> AffineExpression
scaleExpression scale expression =
  AffineExpression
    { expressionCoefficients = Map.map (P.* scale) expression.expressionCoefficients,
      expressionConstant = expression.expressionConstant P.* scale
    }

multiplyExpressions :: AffineExpression -> AffineExpression -> Maybe AffineExpression
multiplyExpressions expression1 expression2 =
  case (constantExpressionValue expression1, constantExpressionValue expression2) of
    (Just constant1, _) -> Just $ scaleExpression constant1 expression2
    (_, Just constant2) -> Just $ scaleExpression constant2 expression1
    _ -> Nothing

constantExpressionValue :: AffineExpression -> Maybe Rational
constantExpressionValue expression
  | Map.null expression.expressionCoefficients = Just expression.expressionConstant
  | otherwise = Nothing

lineariseExactExpr :: ExprStore -> ExprHash -> Maybe AffineExpression
lineariseExactExpr exprNodes root =
  Map.findWithDefault Nothing root
    $ evalExprWith
      lineariseNode
      Expr.Expr {Expr.nodes = exprNodes, Expr.root = root}
      Map.empty
  where
    lineariseNode _ node =
      case node of
        ExprVar {var} -> Just $ linearVar var
        ExprLit {lit} -> Just $ linearConst lit
        ExprUnary {unop = OpNeg, e1} -> negateExpression <$> e1
        ExprUnary {} -> Nothing
        ExprBinary {binop = OpPlus, e1, e2} -> addExpressions <$> e1 <*> e2
        ExprBinary {binop = OpMinus, e1, e2} -> subtractExpressions <$> e1 <*> e2
        ExprBinary {binop = OpTimes, e1, e2} -> do
          expression1 <- e1
          expression2 <- e2
          multiplyExpressions expression1 expression2
        ExprBinary {binop = OpDivide, e1, e2} -> do
          numerator <- e1
          denominator <- e2
          denominatorValue <- constantExpressionValue denominator
          if denominatorValue == 0
            then Nothing
            else Just $ scaleExpression (P.recip denominatorValue) numerator

class CanLineariseEval r where
  lineariseEvaluatedDifference ::
    ExprStore ->
    Map.Map ExprHash r ->
    ExprHash ->
    ExprHash ->
    Maybe LinearRelaxation

instance CanLineariseEval MP.MPBall where
  lineariseEvaluatedDifference _ _ _ _ = Nothing

instance CanLineariseEval MPAffine where
  lineariseEvaluatedDifference exprNodes exprValues e1 e2 = do
    value1 <- Map.lookup e1 exprValues
    value2 <- Map.lookup e2 exprValues
    sources <- sourceVariables exprNodes exprValues
    pure $ affineToRelaxation sources (value1 - value2)

data SourceVariable = SourceVariable
  { sourceVar :: Var,
    sourceCentre :: Rational,
    sourceRadius :: Rational
  }

sourceVariables :: ExprStore -> Map.Map ExprHash MPAffine -> Maybe (Map.Map ErrorTermId SourceVariable)
sourceVariables exprNodes exprValues =
  P.foldl addSource (Just Map.empty) sources
  where
    sources =
      [ (errorId, SourceVariable {sourceVar = var, sourceCentre = mpFloatToRational affine.centre, sourceRadius = mpFloatToRational radius})
        | (exprHash, ExprVar {var}) <- Map.toList exprNodes,
          Just affine <- [Map.lookup exprHash exprValues],
          [(errorId, radius)] <- [Map.toList affine.errTerms],
          radius /= 0
      ]

    addSource Nothing _ = Nothing
    addSource (Just sourcesSoFar) (errorId, source)
      | Map.member errorId sourcesSoFar = Nothing
      | otherwise = Just $ Map.insert errorId source sourcesSoFar

affineToRelaxation :: Map.Map ErrorTermId SourceVariable -> MPAffine -> LinearRelaxation
affineToRelaxation sources affine =
  LinearRelaxation
    { coefficients = Map.fromListWith (P.+) sourceCoefficients,
      rhs = residualRadius P.- constant
    }
  where
    -- Replace each source noise symbol using x = centre + radius * epsilon.
    -- All other symbols stay independent and contribute their absolute radius.
    (sourceTerms, residualTerms) = P.foldr classifyTerm ([], []) (Map.toList affine.errTerms)

    classifyTerm (errorId, coefficient) (sourceAcc, residualAcc) =
      case Map.lookup errorId sources of
        Just source -> ((source, mpFloatToRational coefficient) : sourceAcc, residualAcc)
        Nothing -> (sourceAcc, mpFloatToRational coefficient : residualAcc)

    sourceCoefficients =
      [ (source.sourceVar, coefficient P./ source.sourceRadius)
        | (source, coefficient) <- sourceTerms
      ]
    centreAdjustment =
      sum
        [ (coefficient P./ source.sourceRadius) P.* source.sourceCentre
          | (source, coefficient) <- sourceTerms
        ]
    constant = mpFloatToRational affine.centre P.- centreAdjustment
    residualRadius = sum (P.abs <$> residualTerms)

mpFloatToRational :: MPFloat -> Rational
mpFloatToRational = rational . dyadic

linearRelaxation ::
  (CanLineariseEval r) =>
  ExprStore ->
  Map.Map ExprHash r ->
  ExprHash ->
  ExprHash ->
  Maybe LinearRelaxation
linearRelaxation exprNodes exprValues e1 e2 =
  case (lineariseExactExpr exprNodes e1, lineariseExactExpr exprNodes e2) of
    (Just expression1, Just expression2) ->
      let difference = subtractExpressions expression1 expression2
       in Just
            LinearRelaxation
              { coefficients = difference.expressionCoefficients,
                rhs = P.negate difference.expressionConstant
              }
    _ -> lineariseEvaluatedDifference exprNodes exprValues e1 e2
