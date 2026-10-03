{-# LANGUAGE UndecidableInstances #-}
{-# HLINT ignore "Use >" #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main (main) where

import AERN2.MP qualified as MP
import AERN2.MP.Affine (MPAffine (..), MPAffineConfig (..))
import BranchAndPrune.BranchAndPrune (Problem (..))
import BranchAndPrune.BranchAndPrune qualified as BP
import Control.Concurrent (MVar, forkIO, modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Exception (evaluate)
import Control.Monad (forever, when)
import Control.Monad.IO.Unlift (MonadIO (liftIO))
import Control.Monad.Logger (runStdoutLoggingT)
import Data.Aeson qualified as A
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import GHC.Generics (Generic)
import GHC.Records
import LPPaver2.BranchAndPrune (LPPBPParams (..), LPPStep, getStepBoxes, lppBranchAndPrune)
import LPPaver2.ExampleProblems (LPPProblemWithParamSpec (..), exampleProblems, exampleProblemsList, substituteParams)
import LPPaver2.Export ()
import LPPaver2.RealConstraints (EvalArithmetic (..), ExprStore, FormStore)
import LPPaver2.RealConstraints.Boxes (BoxStore)
import MixedTypesNumPrelude (convert, convertExactly)
import Network.WebSockets qualified as WS
import ServerState (RunID (..), ServerState (..))
import ServerState qualified
import Prelude

main :: IO ()
main = do
  putStrLn "Starting LPPaver2 server."
  WS.runServer "127.0.0.1" 9160 application

application :: WS.ServerApp
application pending = do
  conn <- WS.acceptRequest pending
  putStrLn "Client connected."
  -- withPingThread conn 30 (return ()) (forever (requestResponse conn))
  stateMVar <- newMVar ServerState.new
  stateChangeHandlersMVar <- newMVar ([] :: [ServerState -> ServerState -> IO ()])
  forever $ requestResponse stateMVar stateChangeHandlersMVar conn

requestResponse :: MVar ServerState -> MVar [ServerState -> ServerState -> IO ()] -> WS.Connection -> IO ()
requestResponse stateMVar stateChangeHandlersMVar conn = do
  putStrLn "waiting for message from client..."
  msg <- WS.receiveData conn :: IO Text
  request <- parseRequest msg
  state <- readMVar stateMVar
  _ <-forkIO $
    handleRequest
      ( RequestHandlerInfo
          { request,
            stateOnRequest = state,
            addStateChangeHandler,
            modifyState,
            respond
          }
      )
  pure ()
  where
    addStateChangeHandler :: (ServerState -> ServerState -> IO ()) -> IO ()
    addStateChangeHandler handler = do
      modifyMVar_ stateChangeHandlersMVar $ \handlers -> do
        pure (handlers ++ [handler])
    modifyState :: (ServerState -> (ServerState, t)) -> IO t
    modifyState fn = do
      modifyMVar stateMVar $ \oldState -> do
        let (newStateLazy, result) = fn oldState
        -- force the new state (strict fields => map spines) so that no thunks accumulate in stateMVar
        newState <- evaluate newStateLazy
        -- execute the handlers for the new state
        handlers <- readMVar stateChangeHandlersMVar
        mapM_ (\handler -> handler oldState newState) handlers
        -- save the new state to stateMVar
        pure (newState, result)
    respond :: Response -> IO ()
    respond response = do
      let responseJSON = A.encode response
      -- putStrLn $ "Sending response: " ++ TL.unpack (TL.decodeUtf8 responseJSON)
      WS.sendTextData conn responseJSON

------------------------
--- Example problems ---
------------------------

data GetExampleProblemsRequest = GetExampleProblemsRequest
  deriving (Generic, Show)

data ExampleProblemsResponse = ExampleProblemsResponse
  { problems :: [(String, LPPProblemWithParamSpec)],
    boxes :: BoxStore
  }
  deriving (Generic)

instance IsRequestResponse GetExampleProblemsRequest where
  type ResponseType GetExampleProblemsRequest = ExampleProblemsResponse
  handleRequest RequestHandlerInfo {modifyState, respond} = do
    putStrLn "handling GetExampleProblemsRequest"
    newState <- modifyState $ \state ->
      let newState = ServerState.addBoxes scopes $ ServerState.addForms problemForms state
       in (newState, newState)
    putStrLn "sending ExampleProblemsResponse"
    respond $ ExampleProblemsResponse {problems = problems, boxes = newState.boxes}
    where
      problems = exampleProblemsList
      scopes = map (\(_, p) -> p.problem.scope) problems
      problemForms = map (\(_, p) -> p.problem.constraint) problems

instance A.FromJSON GetExampleProblemsRequest where
  parseJSON = A.genericParseJSON aesonOptions

instance A.ToJSON ExampleProblemsResponse where
  toEncoding = A.genericToEncoding aesonOptions

--------------------------------
--- Formula/expression nodes ---
--------------------------------

data KeepGettingFormulaNodesRequest = KeepGettingFormulaNodesRequest
  deriving (Generic, Show)

data NewFormulaNodesResponse = NewFormulaNodesResponse
  { exprs :: ExprStore,
    forms :: FormStore
  }
  deriving (Generic)

instance IsRequestResponse KeepGettingFormulaNodesRequest where
  type ResponseType KeepGettingFormulaNodesRequest = NewFormulaNodesResponse
  handleRequest RequestHandlerInfo {stateOnRequest = initState, respond, addStateChangeHandler} = do
    -- Respond immediately with the initial state of formula nodes
    respond $ NewFormulaNodesResponse {exprs = initState.exprs, forms = initState.forms}
    -- Add a state change handler to respond with new formula nodes as they are added
    addStateChangeHandler handler
    where
      handler oldState newState =
        when (not (Map.null newExprs) || not (Map.null newForms)) $ do
          respond $ NewFormulaNodesResponse {exprs = newExprs, forms = newForms}
        where
          newExprs = Map.difference newState.exprs oldState.exprs
          newForms = Map.difference newState.forms oldState.forms

instance A.FromJSON KeepGettingFormulaNodesRequest where
  parseJSON = A.genericParseJSON aesonOptions

instance A.ToJSON NewFormulaNodesResponse where
  toEncoding = A.genericToEncoding aesonOptions

------------------------------------------------
--- Running the solver and returning results ---
------------------------------------------------

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
  handleRequest RequestHandlerInfo {request, modifyState, respond} = do
    -- report solver has started
    respond $ SolverRunStatusUpdate {runId, status = SolverRunning, newSteps = [], newBoxes = Map.empty}
    setLastSentTime runId modifyState -- mark the time of this initial update

    -- run the solver with our steps controller
    _ <- runStdoutLoggingT $ do
      lppBranchAndPrune
        (getEvalArithmetic request.arithmetic)
        (lppStepsController runId modifyState reportProgress) -- accummulates steps and boxes and reports them to the client
        (mkParams request)
    -- report any remaining new steps after the solver has finished
    reportProgress
    -- report solver has finished
    respond $ SolverRunStatusUpdate {runId, status = SolverFinished, newSteps = [], newBoxes = Map.empty}
    where
      runId = request.runId
      -- a helper to report new steps
      reportProgress =
        do
          (newSteps, newBoxes) <- modifyState $ ServerState.processNewSteps runId
          respond $ SolverRunStatusUpdate {runId, status = SolverRunning, newSteps, newBoxes}
          setLastSentTime runId modifyState

setLastSentTime :: RunID -> ModifyState () -> IO ()
setLastSentTime runId modifyState = do
  currentTime <- getCurrentTime
  _ <- modifyState $ \state ->
    let newState = ServerState.setLastSentTime runId currentTime state
     in (newState, ())
  pure ()

lppStepsController :: (MonadIO m) => RunID -> ModifyState (Maybe UTCTime) -> IO () -> BP.StepsController m LPPStep
lppStepsController runId modifyState reportProgress =
  BP.StepsController {reportStep}
  where
    reportStep step = liftIO $ do
      currentTime <- getCurrentTime
      maybeLastSentTime <- modifyState $ \state ->
        -- add the new step to the state
        let updatedState = ServerState.addNewSteps runId [step] (getStepBoxes step) state
            lastSentTime = (updatedState.runs Map.! runId).lastSentTime
         in (updatedState, lastSentTime)
      -- report progress to the client but no more than once every 0.5 seconds
      case maybeLastSentTime of
        Nothing -> do
          return ()
        Just lastSentTime -> do
          when (diffUTCTime currentTime lastSentTime > 0.5) reportProgress

-- putStrLn $ "Step for runId " ++ show runId ++ ": " ++ show step

mkParams :: RunSolverRequest -> LPPBPParams
mkParams request =
  LPPBPParams
    { problem = problemWithSubstitutedParams,
      maxThreads = request.numberOfThreads,
      giveUpAccuracy = convert request.giveUpAccuracy,
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

------------------------------------------------
--- Request/Response boilerplate and parsing ---
------------------------------------------------

type ModifyState t = (ServerState -> (ServerState, t)) -> IO t

data RequestHandlerInfo request = RequestHandlerInfo
  { request :: request,
    stateOnRequest :: ServerState,
    -- | Registers a handler that will be called whenever the server state changes.
    -- | The handler receives the old state and the new state as arguments.
    addStateChangeHandler :: (ServerState -> ServerState -> IO ()) -> IO (),
    modifyState :: forall t. ModifyState t,
    respond :: ResponseType request -> IO ()
  }

class IsRequestResponse request where
  type ResponseType request
  handleRequest ::
    RequestHandlerInfo request ->
    IO ()

data Request
  = RequestGetExampleProblems GetExampleProblemsRequest
  | RequestKeepGettingFormulaNodes KeepGettingFormulaNodesRequest
  | RequestRunSolver RunSolverRequest
  deriving (Generic, Show)

data Response
  = ResponseExampleProblems ExampleProblemsResponse
  | ResponseNewFormulaNodes NewFormulaNodesResponse
  | ResponseSolverRunStatusUpdate SolverRunStatusUpdate
  deriving (Generic)

instance IsRequestResponse Request where
  type ResponseType Request = Response
  handleRequest info = do
    case info.request of
      RequestGetExampleProblems req ->
        handleRequest (delegatedRequestInfo req ResponseExampleProblems info)
      RequestKeepGettingFormulaNodes req ->
        handleRequest (delegatedRequestInfo req ResponseNewFormulaNodes info)
      RequestRunSolver req ->
        handleRequest (delegatedRequestInfo req ResponseSolverRunStatusUpdate info)

delegatedRequestInfo ::
  request2 ->
  (ResponseType request2 -> ResponseType request1) ->
  RequestHandlerInfo request1 ->
  RequestHandlerInfo request2
delegatedRequestInfo request2 response2to1 RequestHandlerInfo {..} =
  RequestHandlerInfo
    { request = request2,
      stateOnRequest = stateOnRequest,
      modifyState = modifyState,
      addStateChangeHandler = addStateChangeHandler,
      respond = respond . response2to1
    }

instance A.FromJSON Request where
  parseJSON = A.genericParseJSON aesonOptions

parseRequest :: Text -> IO Request
parseRequest msg =
  let msgBS = T.encodeUtf8 msg
   in case A.eitherDecodeStrict msgBS of
        Right req -> do
          putStrLn $ "Parsed: " ++ show req
          return req
        Left err -> do
          putStrLn $ "Failed to parse request: " ++ err
          fail $ "Failed to parse request: " ++ err

instance A.ToJSON Response where
  toEncoding = A.genericToEncoding aesonOptions

aesonOptions :: A.Options
aesonOptions = A.defaultOptions
