module LPPaver2.RealConstraints.IntervalAD
  ( IntervalAD (partials),
    evalIntervalAD,
    mpBallBounds,
    subtractIntervalAD,
  )
where

import AERN2.MP (HasPrecision (..), mpBallP)
import AERN2.MP qualified as MP
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

data IntervalAD = IntervalAD
  { value :: MP.MPBall,
    partials :: Map.Map Var MP.MPBall
  }

evalIntervalAD :: ExprStore -> Map.Map ExprHash MP.MPBall -> Map.Map Var MP.MPBall -> ExprHash -> Maybe IntervalAD
evalIntervalAD exprNodes knownValues domains root =
  Map.findWithDefault Nothing root
    $ evalExprWith
      evaluateNode
      Expr.Expr {Expr.nodes = exprNodes, Expr.root = root}
      Map.empty
  where
    literal :: Rational -> MP.MPBall
    literal literalValue = mpBallP precision literalValue
    precision = case Map.elems domains of
      domain : _ -> getPrecision domain
      [] -> MP.defaultPrecision

    evaluateNode exprHash node =
      withKnownValue exprHash <$> calculate node

    calculate node =
      case node of
        ExprVar {var} -> do
          domain <- Map.lookup var domains
          pure IntervalAD {value = domain, partials = Map.singleton var (literal (rational 1))}
        ExprLit {lit} -> pure IntervalAD {value = literal lit, partials = Map.empty}
        ExprUnary {unop = OpNeg, e1} -> negateIntervalAD <$> e1
        ExprUnary {unop = OpSqrt, e1} -> do
          operand <- e1
          if lowerBound operand.value > rational 0
            then do
              let result = sqrt operand.value
              derivativeFactor <- reciprocalBall (literal (rational 2) P.* result)
              pure IntervalAD {value = result, partials = scalePartials derivativeFactor operand.partials}
            else Nothing
        ExprUnary {unop = OpSin, e1} -> do
          operand <- e1
          pure IntervalAD {value = sin operand.value, partials = scalePartials (cos operand.value) operand.partials}
        ExprUnary {unop = OpCos, e1} -> do
          operand <- e1
          pure IntervalAD {value = cos operand.value, partials = scalePartials (P.negate (sin operand.value)) operand.partials}
        ExprBinary {binop = OpPlus, e1, e2} -> addIntervalAD <$> e1 <*> e2
        ExprBinary {binop = OpMinus, e1, e2} -> subtractIntervalAD <$> e1 <*> e2
        ExprBinary {binop = OpTimes, e1, e2} -> multiplyIntervalAD <$> e1 <*> e2
        ExprBinary {binop = OpDivide, e1, e2} -> do
          numerator <- e1
          denominator <- e2
          divideIntervalAD numerator denominator

    withKnownValue exprHash result =
      case Map.lookup exprHash knownValues of
        Just knownValue -> result {value = knownValue}
        Nothing -> result

negateIntervalAD :: IntervalAD -> IntervalAD
negateIntervalAD operand =
  IntervalAD
    { value = P.negate operand.value,
      partials = Map.map P.negate operand.partials
    }

addIntervalAD :: IntervalAD -> IntervalAD -> IntervalAD
addIntervalAD left right =
  IntervalAD
    { value = left.value P.+ right.value,
      partials = Map.unionWith (P.+) left.partials right.partials
    }

subtractIntervalAD :: IntervalAD -> IntervalAD -> IntervalAD
subtractIntervalAD left right = addIntervalAD left (negateIntervalAD right)

multiplyIntervalAD :: IntervalAD -> IntervalAD -> IntervalAD
multiplyIntervalAD left right =
  IntervalAD
    { value = left.value P.* right.value,
      partials = Map.unionWith (P.+) (scalePartials right.value left.partials) (scalePartials left.value right.partials)
    }

divideIntervalAD :: IntervalAD -> IntervalAD -> Maybe IntervalAD
divideIntervalAD numerator denominator = do
  quotient <- divideBalls numerator.value denominator.value
  inverseSquare <- reciprocalSquareBall denominator.value
  pure
    IntervalAD
      { value = quotient,
        partials =
          scalePartials
            inverseSquare
            ( Map.unionWith
                (P.+)
                (scalePartials denominator.value numerator.partials)
                (scalePartials (P.negate numerator.value) denominator.partials)
            )
      }

scalePartials :: MP.MPBall -> Map.Map Var MP.MPBall -> Map.Map Var MP.MPBall
scalePartials factor = Map.map (factor P.*)

mpBallBounds :: MP.MPBall -> (Rational, Rational)
mpBallBounds ball = (rational lower, rational upper)
  where
    (lower, upper) = MP.endpoints ball

lowerBound :: MP.MPBall -> Rational
lowerBound = P.fst . mpBallBounds

excludesZero :: MP.MPBall -> Bool
excludesZero ball = lower > rational 0 P.|| upper < rational 0
  where
    (lower, upper) = mpBallBounds ball

divideBalls :: MP.MPBall -> MP.MPBall -> Maybe MP.MPBall
divideBalls numerator denominator = (numerator P.*) <$> reciprocalBall denominator

reciprocalBall :: MP.MPBall -> Maybe MP.MPBall
reciprocalBall ball
  | excludesZero ball = Just $ ballFromBounds (getPrecision ball) (P.recip upper) (P.recip lower)
  | otherwise = Nothing
  where
    (lower, upper) = mpBallBounds ball

reciprocalSquareBall :: MP.MPBall -> Maybe MP.MPBall
reciprocalSquareBall ball
  | excludesZero ball =
      Just
        $ ballFromBounds
          (getPrecision ball)
          (P.recip (largestMagnitude P.* largestMagnitude))
          (P.recip (smallestMagnitude P.* smallestMagnitude))
  | otherwise = Nothing
  where
    (lower, upper) = mpBallBounds ball
    smallestMagnitude = P.min (P.abs lower) (P.abs upper)
    largestMagnitude = P.max (P.abs lower) (P.abs upper)

ballFromBounds :: MP.Precision -> Rational -> Rational -> MP.MPBall
ballFromBounds precision lower upper =
  MP.fromEndpointsAsIntervals
    (mpBallP precision lower)
    (mpBallP precision upper)
