{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use >" #-}

module LPPaver2.ExampleProblems
  ( exampleProblems,
    exampleProblemsList,
    LPPProblemWithParamSpec (..),
    ParamSpec (..),
    substituteParams,
  )
where

import BranchAndPrune.BranchAndPrune (Problem (..))
import Data.Map qualified as Map
import GHC.Generics (Generic)
import GHC.Records (HasField (..))
import LPPaver2.BranchAndPrune (LPPProblem)
import LPPaver2.RealConstraints
import MixedTypesNumPrelude

data LPPProblemWithParamSpec = LPPProblemWithParamSpec
  { problem :: LPPProblem,
    paramSpecs :: [ParamSpec]
  }
  deriving (Generic)

data ParamSpec = ParamSpec
  { paramName :: String,
    defaultValue :: Rational,
    minValue :: Rational,
    maxValue :: Rational
  }
  deriving (Generic)

substituteParams :: LPPProblem -> Map.Map Var Rational -> LPPProblem
substituteParams prob paramValues =
  prob {scope = newScope}
  where
    newScope = addParamValuesToBox paramValues prob.scope

noParams :: LPPProblem -> LPPProblemWithParamSpec
noParams prob =
  LPPProblemWithParamSpec
    { problem = prob,
      paramSpecs = []
    }

epsParam :: LPPProblem -> LPPProblemWithParamSpec
epsParam prob =
  LPPProblemWithParamSpec
    { problem = prob,
      paramSpecs =
        [ ParamSpec
            { paramName = "eps",
              defaultValue = 0.1,
              minValue = 0.0,
              maxValue = 1.0
            }
        ]
    }

exampleProblems :: Map.Map String LPPProblemWithParamSpec
exampleProblems =
  Map.fromList exampleProblemsList

exampleProblemsList :: [(String, LPPProblemWithParamSpec)]
exampleProblemsList =
    [ ( "transitivityEps",
        epsParam
          $ Problem
            { scope = mkBox [("x", (0.0, 2.0)), ("y", (0.0, 2.0)), ("z", (0.0, 2.0))],
              constraint = (((x + eps) <= y) && (y <= z)) `formImpl` (x <= z)
            }
      ),
      ( "simpleAnd",
        noParams
          $ Problem
            { scope = mkBox [("x", (0.0, 2.0)), ("y", (0.0, 2.0))],
              constraint = (y <= exprLit 1.25) && (exprLit 1.25 <= x)
            }
      ),
      ( "simpleAndWithSine",
        noParams
          $ Problem
            { scope = mkBox [("x", (0.0, 2.0)), ("y", (0.0, 2.0))],
              constraint = (y <= exprLit 1.25) && (exprLit 1.25 <= x) && (y <= sin (10.0 * x))
            }
      ),
      ( "circle",
        noParams
          $ Problem
            { scope = mkBox [("x", (0.0, 1.0)), ("y", (0.0, 1.0))],
              constraint = (x * x + y * y <= 1.0)
            }
      ),
      ( "circleEps",
        epsParam
          $ Problem
            { scope = mkBox [("x", (0.0, 1.0)), ("y", (0.0, 1.0))],
              constraint = (x * x + y * y <= 1.0) `formImpl` (x * x + y * y <= 1.0 + eps)
            }
      ),
      ( "circleEpsSqrt",
        epsParam
          $ Problem
            { scope = mkBox [("x", (0.0, 1.0)), ("y", (0.0, 1.0))],
              constraint = (sqrt (x * x + y * y) <= 1.0) || (sqrt (x * x + y * y) > 1.0 - eps)
            }
      ),
      ( "quadraticReduction",
        noParams
          $ Problem
            { scope = mkBox [("x", (-1.0, 1.0)), ("y", (-1.0, 1.0))],
              constraint = 2.0 * x * x - 4.0 * x + 2.0 + y <= (-4.0) * (x - 1.0) + y
            }
      ),
      ( "cubicReduction",
        epsParam
          $ Problem
            { scope = mkBox [("x", (-1.0, 1.0)), ("y", (-1.0, 1.0))],
              constraint = 6.0 * x * x * x + x * x - 10.0 * x + 3.0 + y <= (x - 1.0) * (x - 4.5) + y + eps
            }
      ),
      ( "vcApproxSinLE",
        epsParam
          $ Problem
            { scope = mkBox [("r1", ((-3819831) / 4194304, 7639661 / 8388608)), ("x", ((-6851933) / 8388608, 6851933 / 8388608))],
              constraint =
                let t =
                      ( ( x
                            * ( ( ( ( (((-3350387) / 17179869184) * (x * x))
                                        + (4473217 / 536870912)
                                    )
                                      * (x * x)
                                  )
                                    + ((-349525) / 2097152)
                                )
                                  * (x * x)
                              )
                        )
                          + x
                      )
                 in ( if x <= 1 / 67108864 && -x <= 1 / 67108864
                        then r1 == x
                        else
                          (r1 <= t + (4498891 / 100000000000000))
                            && ((t - (4498891 / 100000000000000)) <= r1)
                    )
                      && not ((r1 + ((-1.0) * sin x)) <= (58 * (1 / 1000000000)) + eps)
            }
      )
    ]
  where
    eps = exprVar "eps" :: Expr
    x = exprVar "x" :: Expr
    y = exprVar "y" :: Expr
    z = exprVar "z" :: Expr
    r1 = exprVar "r1" :: Expr