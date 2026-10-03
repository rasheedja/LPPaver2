{-# LANGUAGE OverloadedStrings #-}

module LPPaver2.Export (lppProblemToJSON) where

import AERN2.Kleenean (Kleenean)
import AERN2.MP qualified as MP
import AERN2.MP.Affine (ErrorTermId (..), MPAffine (..))
import AERN2.MP.Float qualified as MP
import BranchAndPrune.BranchAndPrune qualified as BP
import Data.Aeson (ToJSON (toJSON), (.=))
import Data.Aeson qualified as A
import Data.Aeson.Key (fromString)
import Data.Aeson.Types qualified as A
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as B
import Data.Text.Lazy.Builder.Int qualified as B
import GHC.Records (getField)
import LPPaver2.BranchAndPrune (LPPPaving, LPPProblem)
import LPPaver2.ExampleProblems (LPPProblemWithParamSpec (..), ParamSpec (..))
import LPPaver2.RealConstraints
import LPPaver2.RealConstraints.Eval (EvaluatedFormR (..))
import MixedTypesNumPrelude

------------------------------------------------
-- Serialisation of real number approximations
------------------------------------------------

instance A.ToJSON MP.MPBall where
  toJSON b = A.object ["l" .= lD, "u" .= uD]
    where
      (l, u) = MP.endpoints b
      lD = double l
      uD = double u

instance A.ToJSON MPAffine where
  toJSON (MPAffine {centre, errTerms}) =
    A.object
      [ "center" .= centre,
        "errTerms" .= errTerms
      ]

instance A.ToJSON MP.MPFloat where
  toJSON f = A.Number (realToFrac f)

instance A.ToJSON ErrorTermId where
  toJSON (ErrorTermId i) = A.String (T.pack $ show i)

instance A.ToJSONKey ErrorTermId where
  toJSONKey = A.toJSONKeyText (T.pack . show . \(ErrorTermId i) -> i)

------------------------------------------------
-- Serialisation of LPPaver2 data types
------------------------------------------------

instance A.ToJSON Box_ where
  toEncoding = A.genericToEncoding A.defaultOptions

instance A.ToJSON BoxHash where
  toJSON (BoxHash h) = A.String (intToText h)

instance A.ToJSONKey BoxHash where
  toJSONKey = A.toJSONKeyText (intToText . \(BoxHash h) -> h)

intToText :: Int -> T.Text
intToText = TL.toStrict . B.toLazyText . B.decimal

instance A.ToJSON Box where
  toEncoding = A.genericToEncoding A.defaultOptions

instance A.ToJSON Boxes where
  toJSON (Boxes {store}) =
    A.object ["boxes" .= Map.keys store]

instance A.ToJSON UnaryOp where
  toEncoding = A.genericToEncoding A.defaultOptions

instance A.ToJSON BinaryOp where
  toEncoding = A.genericToEncoding A.defaultOptions

instance (A.ToJSON expr) => A.ToJSON (ExprF expr) where
  toEncoding = A.genericToEncoding A.defaultOptions

instance A.ToJSON ExprHash where
  toJSON (ExprHash h) = A.String (intToText h)

instance A.ToJSONKey ExprHash where
  toJSONKey = A.toJSONKeyText (intToText . \(ExprHash h) -> h)

instance A.ToJSON Expr where
  toJSON (Expr {root}) =
    A.object ["exprH" .= root]

instance A.ToJSON BinaryComp where
  toEncoding = A.genericToEncoding A.defaultOptions

instance A.ToJSON UnaryConn where
  toEncoding = A.genericToEncoding (A.defaultOptions {A.tagSingleConstructors = True})

instance A.ToJSON BinaryConn where
  toEncoding = A.genericToEncoding A.defaultOptions

instance (A.ToJSON form) => A.ToJSON (FormF form) where
  toEncoding = A.genericToEncoding A.defaultOptions

instance A.ToJSON FormHash where
  toJSON (FormHash h) = A.String (intToText h)

instance A.ToJSONKey FormHash where
  toJSONKey = A.toJSONKeyText (intToText . \(FormHash h) -> h)

instance A.ToJSON Form where
  toJSON (Form {root}) =
    A.object ["formH" .= root]

instance A.ToJSON EvaluatedForm where
  toJSON (EvaluatedFormMPBall (EvaluatedFormR {exprValues, formValues})) =
    A.object
      ["exprValues" .= exprValues, "formValues" .= formValues]
  toJSON (EvaluatedFormAffine (EvaluatedFormR {exprValues, formValues})) =
    A.object
      ["exprValues" .= exprValues, "formValues" .= formValues]

instance A.ToJSON Kleenean where
  toEncoding = A.genericToEncoding A.defaultOptions

instance (A.ToJSON problem, A.ToJSON paving, A.ToJSON evalInfo) => A.ToJSON (BP.Step problem paving evalInfo) where
  toEncoding = A.genericToEncoding A.defaultOptions

instance A.ToJSON LPPProblem where
  toJSON = lppProblemToJSON

lppProblemToJSON :: LPPProblem -> A.Value
lppProblemToJSON (BP.Problem {scope, constraint}) =
  A.object ["scope" .= scope.boxHash, "constraint" .= constraint.root]

instance A.ToJSON LPPProblemWithParamSpec where
  toEncoding = A.genericToEncoding A.defaultOptions

instance A.ToJSON ParamSpec where
  toJSON = paramSpecToJSON

paramSpecToJSON :: ParamSpec -> A.Value
paramSpecToJSON (ParamSpec {paramName, defaultValue, minValue, maxValue}) =
  A.object
    [ "paramName" .= paramName,
      "defaultValue" .= rationalToDouble defaultValue,
      "minValue" .= rationalToDouble minValue,
      "maxValue" .= rationalToDouble maxValue
    ]

rationalToDouble :: Rational -> Double
rationalToDouble = realToFrac

instance A.ToJSON LPPPaving where
  toJSON = lppPavingToJSON

lppPavingToJSON :: LPPPaving -> A.Value
lppPavingToJSON (BP.Paving {scope, inner, outer, undecided}) =
  A.object
    [ "scope" .= scope.boxHash,
      "inner" .= inner,
      "outer" .= outer,
      "undecided" .= A.listValue lppProblemToJSON undecided
    ]
