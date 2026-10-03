module ServerState
  ( RunID (..),
    RunInfo (..),
    ServerState (..),
    new,
    addBoxes,
    addForms,
    addNewSteps,
    setLastSentTime,
    processNewSteps,
  )
where

import Data.Aeson qualified as A
import Data.Foldable (toList)
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Time.Clock (UTCTime)
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
    steps :: !(Seq LPPStep),
    newSteps :: !(Seq LPPStep),
    newBoxes :: !BoxStore,
    lastSentTime :: !(Maybe UTCTime) -- the time of the last update sent to the client for this run
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

addNewSteps :: RunID -> [LPPStep] -> BoxStore -> ServerState -> ServerState
addNewSteps runId newSteps newBoxes state =
  addForms newForms $
  addBoxes (Map.elems newBoxes) $
  state {runs = Map.insert runId updatedRunInfo state.runs}
  where
    newForms = List.map (.constraint) $ List.concatMap BP.getStepProblems newSteps
    updatedRunInfo =
      case Map.lookup runId state.runs of
        Nothing ->
          RunInfo
            { runID = runId,
              steps = Seq.empty,
              newSteps = Seq.fromList newSteps,
              newBoxes = newBoxes,
              lastSentTime = Nothing
            }
        Just runInfo ->
          runInfo
            { newSteps = runInfo.newSteps <> Seq.fromList newSteps,
              newBoxes = runInfo.newBoxes `Map.union` newBoxes
            }

setLastSentTime :: RunID -> UTCTime -> ServerState -> ServerState
setLastSentTime runId time state =
  state {runs = Map.insert runId updatedRunInfo state.runs}
  where
    updatedRunInfo =
      case Map.lookup runId state.runs of
        Nothing ->
          RunInfo
            { runID = runId,
              steps = Seq.empty,
              newSteps = Seq.empty,
              newBoxes = Map.empty,
              lastSentTime = Just time
            }
        Just runInfo ->
          runInfo {lastSentTime = Just time}

processNewSteps :: RunID -> ServerState -> (ServerState, ([LPPStep], BoxStore))
processNewSteps runId state =
  case Map.lookup runId state.runs of
    Nothing -> (state, ([], Map.empty))
    Just runInfo ->
      -- shift the newSteps to steps and clear newSteps, update lastSentTime
      let steps = runInfo.steps <> runInfo.newSteps
          updatedRunInfo =
            RunInfo
              { runID = runInfo.runID,
                steps = steps,
                newSteps = Seq.empty,
                newBoxes = Map.empty,
                lastSentTime = Nothing -- this is set later, after sending the update to the client
              }
          updatedState = state {runs = Map.insert runId updatedRunInfo state.runs}
       in (updatedState, (toList runInfo.newSteps, runInfo.newBoxes)) -- also return the shifted newSteps and newBoxes
