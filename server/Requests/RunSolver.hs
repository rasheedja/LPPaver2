{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}

module Requests.RunSolver
  ( RunSolverRequest (..),
    Arithmetic (..),
    SolverRunStatus (..),
    SolverRunStatusUpdate (..),
  )
where

import AERN2.MP qualified as MP
import AERN2.MP.Affine (MPAffine (..), MPAffineConfig (..))
import BranchAndPrune.BranchAndPrune qualified as BP
import Control.Exception (finally)
import Control.Monad (forM_, unless)
import Control.Monad.IO.Unlift (MonadIO (liftIO))
import Control.Monad.Logger (runStdoutLoggingT)
import Data.Aeson qualified as A
import Data.Map qualified as Map
import GHC.Generics (Generic)
import GHC.Records
import LPPaver2.BranchAndPrune (LPPBPParams (..), LPPPruningMethod (..), LPPStep, getStepBoxes, lppBranchAndPrune)
import LPPaver2.ExampleProblems (LPPProblemWithParamSpec (..), exampleProblems, substituteParams)
import LPPaver2.Export ()
import LPPaver2.RealConstraints (EvalArithmetic (..))
import LPPaver2.RealConstraints.Boxes (BoxStore)
import MixedTypesNumPrelude (convert, convertExactly)
import RequestHandler (HandlerContinuation (..), IsRequestResponse (..), ModifyState, RequestHandlerInfo (..), aesonOptions)
import ServerState (RunID (..))
import ServerState qualified
import System.IO.Unsafe (unsafePerformIO)
import Prelude

data Arithmetic
  = BallArithmetic {precision :: Integer}
  | AffineArithmetic {precision :: Integer, maxTerms :: Int}
  deriving (Generic, Show)

getEvalArithmetic :: Arithmetic -> EvalArithmetic
getEvalArithmetic (BallArithmetic {precision}) =
  EvalArithmeticMPBall {sampleBall = MP.mpBallP (MP.prec precision) (0 :: Integer)}
getEvalArithmetic (AffineArithmetic {precision, maxTerms}) =
  EvalArithmeticAffine
    { sampleAffine =
        MPAffine
          { config = MPAffineConfig {maxTerms = maxTerms, precision = precision},
            centre = convertExactly (0 :: Integer),
            errTerms = Map.empty
          }
    }

data RunSolverRequest = RunSolverRequest
  { runId :: RunID,
    problemName :: String,
    paramValues :: Map.Map String Double,
    arithmetic :: Arithmetic,
    useSimplex :: Bool,
    giveUpAccuracy :: Double,
    numberOfThreads :: Int
  }
  deriving (Generic, Show)

data SolverRunStatus = SolverRunning | SolverFinished
  deriving (Generic, Show)

data SolverRunStatusUpdate = SolverRunStatusUpdate
  { runId :: RunID,
    status :: SolverRunStatus,
    newSteps :: [LPPStep],
    newBoxes :: BoxStore
  }
  deriving (Generic)

instance IsRequestResponse RunSolverRequest where
  type ResponseType RunSolverRequest = SolverRunStatusUpdate
  handleRequest RequestHandlerInfo {request, modifyState, respond, addStateChangeHandler, isConnectionClosed} = do
    stateAtStart <- modifyState $ \state -> let newState = ServerState.startRun runId state in (newState, newState)
    -- report solver has started
    respond $ SolverRunStatusUpdate {runId, status = SolverRunning, newSteps = [], newBoxes = Map.empty}
    -- the publisher thread will report progress, coalescing steps reported in quick succession
    addStateChangeHandler stateAtStart progressHandler

    -- run the solver with our steps controller
    -- whatever happens, mark the run as finished so that the publisher reports any remaining steps and then that the solver has finished
    flip finally (modifyState $ \state -> (ServerState.finishRun runId state, ())) $ do
      result <- runStdoutLoggingT $ do
        lppBranchAndPrune
          ( LPPPruningMethod
              { evalArithmetic = getEvalArithmetic request.arithmetic,
                useSimplex = request.useSimplex
              }
          )
          (lppStepsController runId modifyState) -- accummulates steps and boxes in the server state
          (mkParams request) {shouldAbort = abortWhen isConnectionClosed "client disconnected"}
      forM_ result.aborted $ \reason ->
        putStrLn $ "Solver run " ++ show runId ++ " aborted: " ++ reason
    where
      runId = request.runId
      progressHandler oldState newState = do
        let newSteps = ServerState.getNewRunSteps runId oldState newState
            newBoxes = Map.unions (map getStepBoxes newSteps)
        unless (null newSteps) $
          respond $
            SolverRunStatusUpdate {runId, status = SolverRunning, newSteps, newBoxes}
        case ServerState.isRunFinished runId newState of
          True -> do
            respond $ SolverRunStatusUpdate {runId, status = SolverFinished, newSteps = [], newBoxes = Map.empty}
            pure RemoveHandler
          False -> pure KeepHandler

lppStepsController :: (MonadIO m) => RunID -> ModifyState () -> BP.StepsController m LPPStep
lppStepsController runId modifyState =
  BP.StepsController {reportStep}
  where
    -- only record the step; sending it to the client is done by the publisher thread
    reportStep step = liftIO $ do
      modifyState $ \state -> (ServerState.addNewSteps runId [step] (getStepBoxes step) state, ())

-- | Turn an IO condition into a B&P shouldAbort function.
-- The B&P engine requires a pure function, so we read the condition with unsafePerformIO.
-- This is safe because reading a flag has no side effects and a stale value only delays the abort by a step.
-- NOINLINE and the dependency on the paving argument ensure the condition is re-read on every call
-- instead of being floated out and evaluated only once.
{-# NOINLINE abortWhen #-}
abortWhen :: IO Bool -> String -> paving -> Maybe String
abortWhen condition reason paving =
  unsafePerformIO $ do
    shouldAbort <- paving `seq` condition
    pure $ case shouldAbort of
      True -> Just reason
      False -> Nothing

mkParams :: RunSolverRequest -> LPPBPParams
mkParams request =
  LPPBPParams
    { problem = problemWithSubstitutedParams,
      maxThreads = request.numberOfThreads,
      giveUpAccuracy = convert request.giveUpAccuracy,
      shouldAbort = const Nothing,
      shouldLog = False
    }
  where
    problemWithSubstitutedParams = case Map.lookup request.problemName exampleProblems of
      Just (LPPProblemWithParamSpec {problem}) ->
        let paramValues = Map.map convert request.paramValues
            substitutedProblem = substituteParams problem paramValues
         in substitutedProblem
      Nothing -> error $ "Problem not found: " ++ request.problemName

instance A.FromJSON RunSolverRequest where
  parseJSON = A.genericParseJSON aesonOptions

instance A.FromJSON Arithmetic where
  parseJSON = A.genericParseJSON aesonOptions

instance A.ToJSON SolverRunStatus where
  toEncoding = A.genericToEncoding aesonOptions

instance A.ToJSON SolverRunStatusUpdate where
  toEncoding = A.genericToEncoding aesonOptions
