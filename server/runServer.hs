{-# LANGUAGE UndecidableInstances #-}
{-# HLINT ignore "Use >" #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}

module Main (main) where

import AERN2.MP qualified as MP
import AERN2.MP.Affine (MPAffine (..), MPAffineConfig (..))
import BranchAndPrune.BranchAndPrune (Problem (..))
import BranchAndPrune.BranchAndPrune qualified as BP
import Control.Concurrent (MVar, forkIO, killThread, modifyMVar, modifyMVar_, newEmptyMVar, newMVar, readMVar, takeMVar, threadDelay, tryPutMVar, withMVar)
import Control.Exception (SomeException, evaluate, finally, throwIO, try)
import Control.Monad (forM, forM_, forever, unless, void, when)
import Control.Monad.IO.Unlift (MonadIO (liftIO))
import Control.Monad.Logger (runStdoutLoggingT)
import Data.Aeson qualified as A
import Data.Map qualified as Map
import Data.IORef (IORef, atomicWriteIORef, newIORef, readIORef)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as T
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
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)
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
  connState <- newConnectionState conn
  publisherThread <- forkIO (publisher connState)
  let onDisconnect = do
        putStrLn "Client disconnected, cancelling any running solvers."
        -- running solvers check this flag before each step and abort
        atomicWriteIORef connState.connectionClosedRef True
        -- stop the publisher; this also interrupts any send stuck because the client stopped reading
        killThread publisherThread
        -- now that the socket is free, try to complete the closing handshake, but do not wait for long
        void $ try @SomeException $ timeout 1000000 $ WS.sendClose conn Text.empty
  flip finally onDisconnect $
    forever $ requestResponse connState

-- | Per-connection state shared by the request handlers, the solver threads and the publisher thread.
data ConnectionState = ConnectionState
  { conn :: WS.Connection,
    -- | Serialises all writes to the socket.
    sendLock :: MVar (),
    stateMVar :: MVar ServerState,
    -- | Signalled (non-blocking) whenever the state changes, consumed by the publisher.
    stateChangedMVar :: MVar (),
    stateChangeHandlersMVar :: MVar [StateChangeHandler],
    -- | Set when the connection is closed so that tasks such as running solvers can stop.
    connectionClosedRef :: IORef Bool
  }

newConnectionState :: WS.Connection -> IO ConnectionState
newConnectionState conn = do
  sendLock <- newMVar ()
  stateMVar <- newMVar ServerState.new
  stateChangedMVar <- newEmptyMVar
  stateChangeHandlersMVar <- newMVar []
  connectionClosedRef <- newIORef False
  pure ConnectionState {conn, sendLock, stateMVar, stateChangedMVar, stateChangeHandlersMVar, connectionClosedRef}

-- | A registered state change handler together with the last state it has been shown.
data StateChangeHandler = StateChangeHandler
  { lastSeenState :: ServerState,
    onStateChange :: ServerState -> ServerState -> IO HandlerContinuation
  }

data HandlerContinuation = KeepHandler | RemoveHandler

-- | Minimum time between two rounds of state change notifications.
-- Changes made in the meantime are coalesced into the next round.
publishIntervalMicroseconds :: Int
publishIntervalMicroseconds = 500000

-- | Runs the state change handlers outside the state lock, so that slow handlers
-- (eg sending to a slow client) never block the threads that modify the state (eg the solver).
publisher :: ConnectionState -> IO ()
publisher ConnectionState {stateChangedMVar, stateMVar, stateChangeHandlersMVar} = forever $ do
  takeMVar stateChangedMVar -- wait for a change
  newState <- readMVar stateMVar
  modifyMVar_ stateChangeHandlersMVar $ \handlers ->
    fmap catMaybes $ forM handlers $ \handler -> do
      continuation <- handler.onStateChange handler.lastSeenState newState
      pure $ case continuation of
        KeepHandler -> Just handler {lastSeenState = newState}
        RemoveHandler -> Nothing
  threadDelay publishIntervalMicroseconds

-- | Like WS.receiveData but does not reply to a Close message before throwing CloseRequest.
-- WS.receiveData sends the close reply first, which blocks if another thread is stuck sending
-- to a client that stopped reading (eg a browser tab being reloaded), so the disconnect would go unnoticed.
-- The close reply is sent later, in the disconnect handler of 'application'.
receiveText :: WS.Connection -> IO Text
receiveText conn = do
  msg <- WS.receive conn
  case msg of
    WS.DataMessage _ _ _ dataMessage -> pure (WS.fromDataMessage dataMessage)
    WS.ControlMessage (WS.Close code reason) -> throwIO (WS.CloseRequest code reason)
    WS.ControlMessage (WS.Ping payload) -> do
      -- reply in a separate thread so that a stuck send does not stop us from receiving
      _ <- forkIO $ void $ try @SomeException $ WS.send conn (WS.ControlMessage (WS.Pong payload))
      receiveText conn
    WS.ControlMessage (WS.Pong _) -> receiveText conn

requestResponse :: ConnectionState -> IO ()
requestResponse ConnectionState {conn, sendLock, stateMVar, stateChangedMVar, stateChangeHandlersMVar, connectionClosedRef} = do
  putStrLn "waiting for message from client..."
  msg <- receiveText conn
  request <- parseRequest msg
  state <- readMVar stateMVar
  _ <-
    forkIO $
      handleRequest
        ( RequestHandlerInfo
            { request,
              stateOnRequest = state,
              addStateChangeHandler,
              modifyState,
              respond,
              isConnectionClosed = readIORef connectionClosedRef
            }
        )
  pure ()
  where
    addStateChangeHandler :: ServerState -> (ServerState -> ServerState -> IO HandlerContinuation) -> IO ()
    addStateChangeHandler lastSeenState onStateChange = do
      modifyMVar_ stateChangeHandlersMVar $ \handlers -> do
        pure (handlers ++ [StateChangeHandler {lastSeenState, onStateChange}])
      -- make sure the publisher looks at the new handler even if the state does not change again
      void $ tryPutMVar stateChangedMVar ()
    modifyState :: (ServerState -> (ServerState, t)) -> IO t
    modifyState fn = do
      result <- modifyMVar stateMVar $ \oldState -> do
        let (newStateLazy, result) = fn oldState
        -- force the new state (strict fields => map spines) so that no thunks accumulate in stateMVar
        newState <- evaluate newStateLazy
        pure (newState, result)
      -- notify the publisher without blocking
      void $ tryPutMVar stateChangedMVar ()
      pure result
    respond :: Response -> IO ()
    respond response = do
      let responseJSON = A.encode response
      -- putStrLn $ "Sending response: " ++ TL.unpack (TL.decodeUtf8 responseJSON)
      withMVar sendLock $ \_ -> WS.sendTextData conn responseJSON

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
    addStateChangeHandler initState handler
    where
      handler oldState newState = do
        when (not (Map.null newExprs) || not (Map.null newForms)) $ do
          respond $ NewFormulaNodesResponse {exprs = newExprs, forms = newForms}
        pure KeepHandler
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
          (getEvalArithmetic request.arithmetic)
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

------------------------------------------------
--- Request/Response boilerplate and parsing ---
------------------------------------------------

type ModifyState t = (ServerState -> (ServerState, t)) -> IO t

data RequestHandlerInfo request = RequestHandlerInfo
  { request :: request,
    stateOnRequest :: ServerState,
    -- | Registers a handler that will be called whenever the server state changes.
    -- | The handler receives the old state and the new state as arguments.
    -- | The first argument is the state from which the handler should start computing changes,
    -- | typically stateOnRequest or a state obtained from modifyState.
    -- | Handlers are run by the publisher thread, not by the thread that modifies the state,
    -- | and changes may be coalesced, ie the handler may not see every intermediate state.
    addStateChangeHandler :: ServerState -> (ServerState -> ServerState -> IO HandlerContinuation) -> IO (),
    modifyState :: forall t. ModifyState t,
    respond :: ResponseType request -> IO (),
    -- | Whether the client connection has closed, ie long-running tasks should stop.
    isConnectionClosed :: IO Bool
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
delegatedRequestInfo
  request2
  response2to1
  RequestHandlerInfo
    { stateOnRequest,
      modifyState,
      addStateChangeHandler,
      respond,
      isConnectionClosed
    } =
    RequestHandlerInfo
      { request = request2,
        stateOnRequest = stateOnRequest,
        modifyState = modifyState,
        addStateChangeHandler = addStateChangeHandler,
        respond = respond . response2to1,
        isConnectionClosed = isConnectionClosed
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
