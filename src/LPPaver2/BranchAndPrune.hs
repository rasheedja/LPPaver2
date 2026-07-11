{-# LANGUAGE UndecidableInstances #-}

module LPPaver2.BranchAndPrune
  ( --
    LPPProblem,
    LPPPaving,
    LPPStep,
    LPPBPResult,
    LPPBPParams (..),
    lppBranchAndPrune,
    lppBranchAndPruneSimplex,
    getStepBoxes,
    getStepExprs,
    getStepForms,
  )
where

import AERN2.MP (Kleenean (..), MPBall)
import AERN2.MP qualified as MP
import BranchAndPrune.BranchAndPrune qualified as BP
import BranchAndPrune.ForkUtils (MonadUnliftIOWithState)
import Control.Monad.IO.Unlift (MonadIO)
import Control.Monad.Logger (MonadLogger)
import Data.Hashable (Hashable (hash))
import Data.Map qualified as Map
import GHC.Records
import LPPaver2.LinearPrune (LinearPruneResult (..), linearPruneWithEvalBounds)
import LPPaver2.RealConstraints
import LPPaver2.SimplexPrune (simplexPruneWithEvalBounds)
import MixedTypesNumPrelude
import Text.Printf (printf)
-- import Debug.Trace (trace)

type LPPProblem = BP.Problem Form Box

type LPPPaving = BP.Paving Form Box Boxes

type LPPStep r = BP.Step LPPProblem LPPPaving (EvaluatedForm r)

-- a map of hashes to boxes, for all boxes in the step pavings AND problems
getStepBoxes :: LPPStep r -> BoxStore
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


-- a map of expr hashes to exprs, for all exprs in the step problems AND all exprs in undecided pavings
getStepExprs :: LPPStep r -> ExprStore
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

-- a map of form hashes to forms, for all forms in the step problems AND all forms in undecided pavings
getStepForms :: LPPStep r -> FormStore
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

-- form store with true and false
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

-- give up bpp if all domains are under the threshold
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
      diameter <= giveUpAccuracy
      where
        diameter = 2 * MP.radius ball

lppBranchAndPrune ::
  ( MonadLogger m,
    MonadIO m,
    MonadUnliftIOWithState m,
    CanEval r,
    HasKleeneanComparison r,
    ConvertibleExactly r MP.MPBall,
    BP.CanControlSteps m (LPPStep r)
  ) =>
  r ->
  LPPBPParams ->
  m LPPBPResult
lppBranchAndPrune (sampleR :: r) (LPPBPParams {..}) = do
  BP.branchAndPruneM
    ( BP.Params
        { BP.problem,
          BP.pruningMethod = sampleR,
          BP.shouldAbort = const Nothing,
          BP.shouldGiveUpSolvingProblem = shouldGiveUpOnBPLPPProblem giveUpAccuracy :: LPPProblem -> Bool,
          BP.dummyPriorityQueue,
          BP.dummyEvalInfo = EvaluatedForm {form = formTrue, exprValues = Map.empty, formValues = Map.empty} :: EvaluatedForm r,
          BP.maxThreads,
          BP.shouldLog
        }
    )
  where
    dummyPriorityQueue :: BoxStack
    dummyPriorityQueue = BoxStack [problem]

lppBranchAndPruneSimplex ::
  ( MonadLogger m,
    MonadIO m,
    MonadUnliftIOWithState m,
    CanEval r,
    HasKleeneanComparison r,
    ConvertibleExactly r MP.MPBall,
    BP.CanControlSteps m (LPPStep r)
  ) =>
  r ->
  LPPBPParams ->
  m LPPBPResult
lppBranchAndPruneSimplex (sampleR :: r) (LPPBPParams {..}) =
  BP.branchAndPruneM
    ( BP.Params
        { BP.problem,
          BP.pruningMethod = WithSimplex sampleR,
          BP.shouldAbort = const Nothing,
          BP.shouldGiveUpSolvingProblem = shouldGiveUpOnBPLPPProblem giveUpAccuracy :: LPPProblem -> Bool,
          BP.dummyPriorityQueue = BoxStack [problem],
          BP.dummyEvalInfo = EvaluatedForm {form = formTrue, exprValues = Map.empty, formValues = Map.empty} :: EvaluatedForm r,
          BP.maxThreads,
          BP.shouldLog
        }
    )

instance
  (CanEval r, HasKleeneanComparison r, ConvertibleExactly r MP.MPBall, Applicative m) =>
  BP.CanPrune m r Form Box Boxes (EvaluatedForm r)
  where
  pruneProblemM sampleR (BP.Problem {scope, constraint}) =
    pure (pavingP, simplificationResult.evaluatedForm)
    where
      simplificationResult = simplifyEvalForm sampleR scope constraint
      simplifiedForm = simplificationResult.evaluatedForm.form
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
            case linearPruneWithEvalBounds simplifiedFormProblem simplificationResult.evaluatedForm.exprValues of
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

-- | Wrapper to select simplex-enhanced pruning at the type level.
newtype WithSimplex r = WithSimplex r

instance
  (CanEval r, HasKleeneanComparison r, MonadIO m, ConvertibleExactly r MP.MPBall) =>
  BP.CanPrune m (WithSimplex r) Form Box Boxes (EvaluatedForm r)
  where
  pruneProblemM (WithSimplex (sampleR :: r)) (BP.Problem {scope, constraint}) = do
    let simplificationResult = simplifyEvalForm sampleR scope constraint
        simplifiedForm = simplificationResult.evaluatedForm.form
        -- remove unused variables from the split order:
        simplifiedScope = boxRestrictSplitOrder (formVariables simplifiedForm) scope
        simplifiedFormProblem = BP.Problem {scope = simplifiedScope, constraint = simplifiedForm}
    pavingP <- case getFormDecision simplifiedForm of
      CertainTrue -> pure $ BP.pavingInner scope (mkBoxes scope)
      CertainFalse -> pure $ BP.pavingOuter scope (mkBoxes scope)
      TrueOrFalse -> do
        -- try simplex pruning first
        simplexResult <- simplexPruneWithEvalBounds simplifiedScope simplifiedForm simplificationResult.evaluatedForm.exprValues
        case simplexResult of
          Just linearPruneResult ->
            pure $ mkLinearPrunePaving scope simplifiedForm linearPruneResult
          Nothing ->
            -- fall back to basic linear pruning
            case linearPruneWithEvalBounds simplifiedFormProblem simplificationResult.evaluatedForm.exprValues of
              Just linearPruneResult ->
                pure $ mkLinearPrunePaving scope simplifiedForm linearPruneResult
              Nothing ->
                pure $ BP.pavingUndecided scope [simplifiedFormProblem]
    pure (pavingP, simplificationResult.evaluatedForm)
    where
      mkBoxes box = Boxes {store = Map.fromList [(box.boxHash, box)]}

instance BP.IsPriorityQueue BoxStack LPPProblem where
  singletonQueue e = BoxStack [e]
  queueToList (BoxStack list) = list
  queuePickNext :: BoxStack -> Maybe (LPPProblem, BoxStack)
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
