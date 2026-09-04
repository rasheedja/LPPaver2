{-# LANGUAGE UndecidableInstances #-}

module LPPaver2.BranchAndPrune
  ( --
    LPPProblem,
    LPPPaving,
    LPPStep,
    LPPBPResult,
    LPPBPParams (..),
    LPPPruningMethod (..),
    lppBranchAndPrune,
    getStepBoxes,
    getStepExprs,
    getStepForms,
  )
where

import AERN2.MP (Kleenean (..), MPBall)
import AERN2.MP qualified as MP
import BranchAndPrune.BranchAndPrune qualified as BP
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (MonadLogger)
import Data.Hashable (Hashable (hash))
import Data.Map qualified as Map
import GHC.Records
import LPPaver2.LinearPrune (LinearPruneResult (..), linearPruneWithEvalValues)
import LPPaver2.RealConstraints
import LPPaver2.RealConstraints.Eval (EvaluatedFormR (..))
import LPPaver2.SimplexPrune (CanProvideSimplexRelaxations, simplexPruneWithEvalValues)
import MixedTypesNumPrelude
import Text.Printf (printf)

-- import Debug.Trace (trace)

type LPPProblem = BP.Problem Form Box

type LPPPaving = BP.Paving Form Box Boxes

type LPPStep = BP.Step LPPProblem LPPPaving EvaluatedForm

getStepBoxes :: LPPStep -> BoxStore
getStepBoxes step =
  scopesStore `Map.union` pavingBoxStore
  where
    scopesStore = boxListToStore $ problemsScopes <> pavingsScopes
    boxListToStore :: [Box] -> BoxStore
    boxListToStore boxes = Map.fromList [(box.boxHash, box) | box <- boxes]
    problems = BP.getStepProblems step
    problemsScopes = [p.scope | p <- problems]
    pavings = BP.getStepPavings step
    pavingsScopes = [p.scope | p <- pavings]
    pavingBoxStore = Map.unions [paving.inner.store `Map.union` paving.outer.store | paving <- pavings]

getStepExprs :: LPPStep -> ExprStore
getStepExprs step =
  constraintsStore `Map.union` undecidedStore
  where
    constraintsStore = Map.unions [prob.constraint.nodesE | prob <- problems]
    undecidedStore =
      Map.unions
        [ prob.constraint.nodesE
          | paving <- pavings,
            prob <- paving.undecided
        ]
    problems = BP.getStepProblems step
    pavings = BP.getStepPavings step

getStepForms :: LPPStep -> FormStore
getStepForms step =
  constraintsStore `Map.union` undecidedStore `Map.union` basicFormStore
  where
    constraintsStore = Map.unions [prob.constraint.nodesF | prob <- problems]
    undecidedStore =
      Map.unions
        [ prob.constraint.nodesF
          | paving <- pavings,
            prob <- paving.undecided
        ]
    problems = BP.getStepProblems step
    pavings = BP.getStepPavings step

basicFormStore :: FormStore
basicFormStore =
  Map.fromList
    [ (FormHash (hash (FormTrue :: FormF FormHash)), FormTrue),
      (FormHash (hash (FormFalse :: FormF FormHash)), FormFalse)
    ]

type LPPBPResult = BP.Result Form Box Boxes

data LPPBPParams = LPPBPParams
  { problem :: LPPProblem,
    maxThreads :: Int,
    giveUpAccuracy :: Rational,
    shouldLog :: Bool
  }

data LPPPruningMethod = LPPPruningMethod
  { evalArithmetic :: EvalArithmetic,
    useSimplex :: Bool
  }

shouldGiveUpOnBPLPPProblem :: Rational -> LPPProblem -> Bool
shouldGiveUpOnBPLPPProblem giveUpAccuracy (BP.Problem {scope}) =
  all accuracyBelowThreshold domainsOfSplitVars
  where
    domainsOfSplitVars =
      [ ball
        | var <- scope.box_.splitOrder,
          Just ball <- [Map.lookup var scope.box_.varDomains]
      ]

    accuracyBelowThreshold :: MPBall -> Bool
    accuracyBelowThreshold ball =
      -- trace (printf "Checking if box with radius %s should be given up (threshold: %s)" (show (MP.radius ball)) (show $ double giveUpAccuracy)) $
      diameter <= giveUpAccuracy
      where
        diameter = 2 * MP.radius ball

lppBranchAndPrune ::
  ( MonadLogger m,
    MonadUnliftIO m
  ) =>
  LPPPruningMethod ->
  BP.StepsController m LPPStep ->
  LPPBPParams ->
  m LPPBPResult
lppBranchAndPrune pruningMethod lppStepsController (LPPBPParams {..}) = do
  BP.branchAndPruneM
    lppStepsController
    ( BP.Params
        { BP.problem,
          BP.pruningMethod = pruningMethod,
          BP.shouldAbort = const Nothing,
          BP.shouldGiveUpSolvingProblem = shouldGiveUpOnBPLPPProblem giveUpAccuracy :: LPPProblem -> Bool,
          BP.dummyPriorityQueue,
          BP.dummyEvalInfo = EvaluatedFormMPBall EvaluatedFormR {form = formTrue, exprValues = Map.empty, formValues = Map.empty},
          BP.maxThreads,
          BP.shouldLog
        }
    )
  where
    dummyPriorityQueue :: BoxStack
    dummyPriorityQueue = BoxStack [problem]

instance
  (MonadIO m) =>
  BP.CanPrune m LPPPruningMethod Form Box Boxes EvaluatedForm
  where
  pruneProblemM pruningMethod (BP.Problem {scope, constraint}) = do
    pavingP <-
      -- first see if simple evaluation decides the problem:
      case getFormDecision simplifiedForm of
        CertainTrue -> pure $ BP.pavingInner scope (mkBoxes scope)
        CertainFalse -> pure $ BP.pavingOuter scope (mkBoxes scope)
        _ -> pruneUncertain
    pure (pavingP, simplificationResult.evaluatedForm)
    where
      simplificationResult = simplifyEvalForm pruningMethod.evalArithmetic scope constraint
      evaluatedForm = simplificationResult.evaluatedForm
      simplifiedForm = case evaluatedForm of
        EvaluatedFormMPBall (EvaluatedFormR {form}) -> form
        EvaluatedFormAffine (EvaluatedFormR {form}) -> form
      -- remove unused variables from the split order:
      simplifiedScope = boxRestrictSplitOrder (formVariables simplifiedForm) scope
      simplifiedFormProblem = BP.Problem {scope = simplifiedScope, constraint = simplifiedForm}

      mkBoxes box = Boxes {store = Map.fromList [(box.boxHash, box)]}

      pruneUncertain =
        case evaluatedForm of
          EvaluatedFormMPBall (EvaluatedFormR {exprValues}) ->
            pruneWithEvalValues pruningMethod.useSimplex scope simplifiedFormProblem exprValues
          EvaluatedFormAffine (EvaluatedFormR {exprValues}) ->
            pruneWithEvalValues pruningMethod.useSimplex scope simplifiedFormProblem exprValues

pruneWithEvalValues ::
  (MonadIO m, CanProvideSimplexRelaxations r) =>
  Bool ->
  Box ->
  BP.Problem Form Box ->
  Map.Map ExprHash r ->
  m (BP.Paving Form Box Boxes)
pruneWithEvalValues useSimplex scope simplifiedFormProblem exprValues = do
  maybeSimplexResult <-
    if useSimplex
      then simplexPruneWithEvalValues simplifiedFormProblem.scope simplifiedForm exprValues
      else pure Nothing
  case maybeSimplexResult of
    Just simplexResult -> pure $ mkLinearPrunePaving scope simplifiedForm simplexResult
    Nothing ->
      -- Defensive fallback: simplex currently subsumes all relaxations supported
      -- by linear pruning, but retain this for future solver/relaxation changes.
      case linearPruneWithEvalValues simplifiedFormProblem exprValues of
        Just linearPruneResult ->
          pure $ mkLinearPrunePaving scope simplifiedForm linearPruneResult
        _ ->
          pure $ BP.pavingUndecided scope [simplifiedFormProblem]
  where
    simplifiedForm = simplifiedFormProblem.constraint

mkLinearPrunePaving :: Box -> Form -> LinearPruneResult -> BP.Paving Form Box Boxes
mkLinearPrunePaving scope simplifiedForm LinearPruneResult {maybeRemainingBox, removedRegionTruth} =
  case maybeRemainingBox of
    Nothing ->
      -- linear pruning decided the whole box
      if removedRegionTruth
        then BP.pavingInner scope (mkBoxes scope) -- true on scope
        else BP.pavingOuter scope (mkBoxes scope) -- false on scope
    Just remainingBox ->
      -- linear pruning
      let remainingProblem = BP.Problem {scope = remainingBox, constraint = simplifiedForm}
          decidedBoxes = mkBoxes $ mkBoxDifference scope remainingBox
       in BP.Paving
            { scope,
              inner = if removedRegionTruth then decidedBoxes else BP.emptySet,
              outer = if removedRegionTruth then BP.emptySet else decidedBoxes,
              undecided = [remainingProblem]
            }
  where
    mkBoxes box = Boxes {store = Map.fromList [(box.boxHash, box)]}

newtype BoxStack = BoxStack [LPPProblem]

instance BP.IsPriorityQueue BoxStack LPPProblem where
  singletonQueue e = BoxStack [e]
  queueToList (BoxStack list) = list
  queuePickNext (BoxStack []) = Nothing
  queuePickNext (BoxStack (e : es)) = Just (e, BoxStack es)
  queueAddMany (BoxStack es) new_es = BoxStack (new_es ++ es)
  queueSplit (BoxStack es)
    | splitPoint == 0 = Nothing
    | otherwise = Just (BoxStack esL, BoxStack esR)
    where
      splitPoint = length es `divI` 2
      (esL, esR) = splitAt splitPoint es

  queueMerge (BoxStack stackL) (BoxStack stackR) = BoxStack $ stackL ++ stackR

instance BP.ShowStats (BP.Subset Boxes Box) where
  showStats (BP.Subset {..}) =
    printf "{|boxes| = %d, coverage = %3.4f%%}" (boxesCount subset) coveragePercent
    where
      coveragePercent = 100 * (boxesAreaD subset / boxAreaD superset)

instance BP.IsSet Boxes where
  emptySet = Boxes {store = Map.empty}
  setIsEmpty (Boxes {store}) = Map.null store
  setUnion bs1 bs2 = Boxes {store = Map.union bs1.store bs2.store}

instance BP.BasicSetsToSet Box Boxes where
  basicSetsToSet list = Boxes {store}
    where
      store = Map.fromList [(box.boxHash, box) | box <- list]

instance BP.CanSplitProblem Form Box where
  splitProblem :: BP.Problem Form Box -> [BP.Problem Form Box]
  splitProblem (BP.Problem {scope, constraint}) =
    map (\box -> BP.Problem {scope = box, constraint}) $ splitBox scope
