module RequestHandler
  ( ModifyState,
    HandlerContinuation (..),
    RequestHandlerInfo (..),
    IsRequestResponse (..),
    delegatedRequestInfo,
    aesonOptions,
  )
where

import Data.Aeson qualified as A
import ServerState (ServerState (..))
import Prelude

type ModifyState t = (ServerState -> (ServerState, t)) -> IO t

data HandlerContinuation = KeepHandler | RemoveHandler

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

aesonOptions :: A.Options
aesonOptions = A.defaultOptions
