module Connection
  ( ConnectionState (..),
    newConnectionState,
    StateChangeHandler (..),
    publisher,
    receiveText,
  )
where

import Control.Concurrent (MVar, forkIO, modifyMVar_, newEmptyMVar, newMVar, readMVar, takeMVar, threadDelay)
import Control.Exception (SomeException, throwIO, try)
import Control.Monad (forM, forever, void)
import Data.IORef (IORef, newIORef)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import GHC.Records
import Network.WebSockets qualified as WS
import RequestHandler (HandlerContinuation (..))
import ServerState (ServerState (..))
import ServerState qualified
import Prelude

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
-- The close reply is sent later, in the disconnect handler of 'Main.application'.
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
