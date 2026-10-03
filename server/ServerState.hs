module ServerState
  ( RunID (..),
    RunInfo (..),
    ServerState (..),
    new,
    addBoxes,
    addForms,
    addNewSteps,
    startRun,
    finishRun,
    getNewRunSteps,
    isRunFinished,
  )
where

import Data.Aeson qualified as A
import Data.Foldable (toList)
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import GHC.Generics (Generic)
import GHC.Records
import BranchAndPrune.BranchAndPrune qualified as BP
import LPPaver2.BranchAndPrune (LPPStep)
import LPPaver2.RealConstraints (Box (..), BoxStore, ExprStore, Form (..), FormStore)
import Prelude

-- All fields are strict so that forcing a state to WHNF also forces the spines of its maps,
-- preventing thunk build-up inside the state MVar.
data ServerState = ServerState
  { boxes :: !BoxStore,
    exprs :: !ExprStore,
    forms :: !FormStore,
    runs :: !(Map.Map RunID RunInfo)
  }

newtype RunID = RunID String
  deriving (Eq, Ord, Show, Generic)

instance A.FromJSON RunID where
  parseJSON = A.genericParseJSON A.defaultOptions

instance A.ToJSON RunID where
  toEncoding = A.genericToEncoding A.defaultOptions

data RunInfo = RunInfo
  { runID :: !RunID,
    steps :: !(Seq LPPStep), -- all steps reported so far, in order
    finished :: !Bool
  }

new :: ServerState
new =
  ServerState
    { boxes = Map.empty,
      exprs = Map.empty,
      forms = Map.empty,
      runs = Map.empty
    }

addBoxes :: [Box] -> ServerState -> ServerState
addBoxes newBoxes state =
  state {boxes = state.boxes `Map.union` newBoxesMap}
  where
    newBoxesMap = Map.fromList [(b.boxHash, b) | b <- newBoxes]

addForms :: [Form] -> ServerState -> ServerState
addForms newForms state =
  state
    { exprs = Map.unions $ state.exprs : newExprNodes,
      forms = Map.unions $ state.forms : newFormNodes
    }
  where
    newExprNodes = List.map (\f -> f.nodesE) newForms
    newFormNodes = List.map (\f -> f.nodesF) newForms

-- | Record new steps for a run, together with their boxes and formula nodes.
-- This is called by the solver threads for every step, so it needs to be cheap.
addNewSteps :: RunID -> [LPPStep] -> BoxStore -> ServerState -> ServerState
addNewSteps runId newSteps newBoxes state =
  addForms newForms $
    addBoxes (Map.elems newBoxes) $
      state {runs = Map.alter (Just . addSteps) runId state.runs}
  where
    newForms = List.map (.constraint) $ List.concatMap BP.getStepProblems newSteps
    addSteps Nothing = RunInfo {runID = runId, steps = Seq.fromList newSteps, finished = False}
    addSteps (Just runInfo) = runInfo {steps = runInfo.steps <> Seq.fromList newSteps}

startRun :: RunID -> ServerState -> ServerState
startRun runId state =
  state {runs = Map.insert runId RunInfo {runID = runId, steps = Seq.empty, finished = False} state.runs}

finishRun :: RunID -> ServerState -> ServerState
finishRun runId state =
  state {runs = Map.adjust (\runInfo -> runInfo {finished = True}) runId state.runs}

-- | The steps of the run that are in the new state but not in the old state.
-- Cheap, because steps are only ever appended.
getNewRunSteps :: RunID -> ServerState -> ServerState -> [LPPStep]
getNewRunSteps runId oldState newState =
  case Map.lookup runId newState.runs of
    Nothing -> []
    Just newRunInfo -> toList $ Seq.drop oldCount newRunInfo.steps
  where
    oldCount = maybe 0 (Seq.length . (.steps)) (Map.lookup runId oldState.runs)

isRunFinished :: RunID -> ServerState -> Bool
isRunFinished runId state = maybe False (.finished) (Map.lookup runId state.runs)
