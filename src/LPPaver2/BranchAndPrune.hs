{-# LANGUAGE UndecidableInstances #-}

module LPPaver2.BranchAndPrune
  ( --
    LPPProblem,
    LPPPaving,
    LPPStep,
    LPPBPResult,
    LPPBPParams (..),
    lppBranchAndPrune,
    getStepBoxes,
    getStepExprs,
    getStepForms,
  )
where

import AERN2.MP (Kleenean (..), MPBall)
import AERN2.MP qualified as MP
import BranchAndPrune.BranchAndPrune qualified as BP
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (MonadLogger)
import Data.Hashable (Hashable (hash))
import Data.Map qualified as Map
import GHC.Records
import LPPaver2.LinearPrune (LinearPruneResult (..), linearPrune)
import LPPaver2.RealConstraints
import LPPaver2.RealConstraints.Eval (EvaluatedFormR (..))
import MixedTypesNumPrelude
import Text.Printf (printf)

-- import Debug.Trace (trace)

type LPPProblem = BP.Problem Form Box

type LPPPaving = BP.Paving Form Box Boxes

type LPPStep = BP.Step LPPProblem LPPPaving EvaluatedForm

getStepBoxes :: LPPStep -> BoxStore
getStepBoxes step =
  Map.unions $ scopesStore : concat [[paving.inner.store, paving.outer.store] | paving <- pavings]
  where
    pavings = BP.getStepPavings step
    scopes = [p.scope | p <- BP.getStepProblems step] <> [p.scope | p <- pavings]
    scopesStore = Map.fromList [(box.boxHash, box) | box <- scopes]

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
  EvalArithmetic ->
  BP.StepsController m LPPStep ->
  LPPBPParams ->
  m LPPBPResult
lppBranchAndPrune evalArithmetic lppStepsController (LPPBPParams {..}) = do
  BP.branchAndPruneM
    lppStepsController
    ( BP.Params
        { BP.problem,
          BP.pruningMethod = evalArithmetic,
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
  (Applicative m) =>
  BP.CanPrune m EvalArithmetic Form Box Boxes EvaluatedForm
  where
  pruneProblemM evalArithmetic (BP.Problem {scope, constraint}) =
    pure (pavingP, simplificationResult.evaluatedForm)
    where
      simplificationResult = simplifyEvalForm evalArithmetic scope constraint
      simplifiedForm = case simplificationResult.evaluatedForm of
        EvaluatedFormMPBall (EvaluatedFormR {form}) -> form
        EvaluatedFormAffine (EvaluatedFormR {form}) -> form
      -- remove unused variables from the split order:
      simplifiedScope = boxRestrictSplitOrder (formVariables simplifiedForm) scope
      simplifiedFormProblem = BP.Problem {scope = simplifiedScope, constraint = simplifiedForm}

      pavingP =
        -- first see if simple evaluation decides the problem:
        case getFormDecision simplifiedForm of
          CertainTrue -> BP.pavingInner scope (mkBoxes scope)
          CertainFalse -> BP.pavingOuter scope (mkBoxes scope)
          _ ->
            -- if not decided, see if linear pruning can decide the problem or at least reduce the box:
            case linearPrune simplifiedFormProblem of
              Just linearPruneResult ->
                -- if linear pruning can help, return the paving with the reduced box and simplified form:
                mkLinearPrunePaving scope simplifiedForm linearPruneResult
              _ ->
                -- if linear pruning cannot help, return the simplified problem as undecided with unchanged scope:
                BP.pavingUndecided scope [simplifiedFormProblem]
        where
          mkBoxes box = Boxes {store = Map.fromList [(box.boxHash, box)]}

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
