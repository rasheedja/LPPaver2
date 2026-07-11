module LPPaver2.SimplexPruneSpec (spec) where

import Data.Map qualified as Map
import GHC.Records (HasField (getField))
import LPPaver2.RealConstraints.Boxes (mkBox)
import LPPaver2.RealConstraints.Expr (Expr (..), exprLit, exprVar)
import LPPaver2.SimplexPrune (simplexPrune)
import MixedTypesNumPrelude
import Test.Hspec

x :: Expr
x = exprVar "x"

y :: Expr
y = exprVar "y"

spec :: Spec
spec = describe "simplexPrune" $ do
  it "uses interval fallback for unsupported nonlinear terms" $ do
    let sinX = sin x
        form = sinX + y <= exprLit 1.0
        box = mkBox [("x", (0.0, 1.0)), ("y", (0.0, 1.0))]
        exprBounds =
          Map.fromList
            [ (sinX.root, (-1.0, 1.0))
            ]

    result <- simplexPrune box form exprBounds
    case result of
      Nothing -> pure ()
      Just _ -> expectationFailure "expected no simplex pruning improvement"
