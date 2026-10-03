{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use >" #-}

module Requests.FormulaNodes
  ( KeepGettingFormulaNodesRequest (..),
    NewFormulaNodesResponse (..),
  )
where

import Control.Monad (when)
import Data.Aeson qualified as A
import Data.Map qualified as Map
import GHC.Generics (Generic)
import GHC.Records
import LPPaver2.Export ()
import LPPaver2.RealConstraints (ExprStore, FormStore)
import RequestHandler (HandlerContinuation (..), IsRequestResponse (..), RequestHandlerInfo (..), aesonOptions)
import ServerState (ServerState (..))
import Prelude

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
