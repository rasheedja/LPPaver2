module Requests.ExampleProblems
  ( GetExampleProblemsRequest (..),
    ExampleProblemsResponse (..),
  )
where

import BranchAndPrune.BranchAndPrune (Problem (..))
import Data.Aeson qualified as A
import GHC.Generics (Generic)
import GHC.Records
import LPPaver2.ExampleProblems (LPPProblemWithParamSpec (..), exampleProblemsList)
import LPPaver2.Export ()
import LPPaver2.RealConstraints.Boxes (BoxStore)
import RequestHandler (IsRequestResponse (..), RequestHandlerInfo (..), aesonOptions)
import ServerState (ServerState (..))
import ServerState qualified
import Prelude

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
