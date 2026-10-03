module Main (main) where

import Connection (ConnectionState (..), StateChangeHandler (..), newConnectionState, publisher, receiveText)
import Control.Concurrent (forkIO, killThread, modifyMVar, modifyMVar_, readMVar, tryPutMVar, withMVar)
import Control.Exception (SomeException, evaluate, finally, try)
import Control.Monad (forever, void)
import Data.Aeson qualified as A
import Data.IORef (atomicWriteIORef, readIORef)
import Data.Text qualified as Text
import GHC.Records
import Network.WebSockets qualified as WS
import RequestHandler (HandlerContinuation, IsRequestResponse (..), RequestHandlerInfo (..))
import Requests (Response, parseRequest)
import ServerState (ServerState (..))
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
    forever $
      requestResponse connState

requestResponse :: ConnectionState -> IO ()
requestResponse ConnectionState {conn, sendLock, stateMVar, stateChangedMVar, stateChangeHandlersMVar, connectionClosedRef} = do
  putStrLn "waiting for message from client..."
  msg <- receiveText conn
  request <- parseRequest msg
  state <- readMVar stateMVar
  void $
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
