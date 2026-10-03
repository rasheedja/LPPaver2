module Requests
  ( Request (..),
    Response (..),
    parseRequest,
  )
where

import Data.Aeson qualified as A
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import GHC.Generics (Generic)
import GHC.Records
import RequestHandler (IsRequestResponse (..), RequestHandlerInfo (..), aesonOptions, delegatedRequestInfo)
import Requests.ExampleProblems (ExampleProblemsResponse, GetExampleProblemsRequest)
import Requests.FormulaNodes (KeepGettingFormulaNodesRequest, NewFormulaNodesResponse)
import Requests.RunSolver (RunSolverRequest, SolverRunStatusUpdate)
import Prelude

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
