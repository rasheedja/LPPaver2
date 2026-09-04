import type { Box, BoxHash } from '@/boxes/boxes';
import type { ExprF, ExprHash } from '@/formulas/exprs';
import type { FormF, FormHash } from '@/formulas/forms';
import type { Problem } from '@/problems/problems';
import type { Step } from '@/steps/steps';
import type { Websocket } from 'websocket-ts';

// geting example problems

export type GetExampleProblemsRequest = [];

export type ExampleProblemsResponse = {
  problems: Array<[string, ProblemWithParamSpec]>;
  boxes: Record<BoxHash, Box>;
};

export type ProblemWithParamSpec = {
  problem: Problem;
  paramSpecs: ParamSpec[];
};

export type ParamSpec = {
  paramName: string;
  defaultValue: number;
  minValue: number;
  maxValue: number;
};

// getting formula nodes

export type GetAllFormulaNodesRequest = [];

export type FormulaNodesResponse = {
  exprs: Record<ExprHash, ExprF<ExprHash>>;
  forms: Record<FormHash, FormF<ExprHash, FormHash>>;
};

// running solver

export type Arithmetic =
  | { tag: 'BallArithmetic'; precision: number }
  | { tag: 'AffineArithmetic'; precision: number; maxTerms: number };

export type RunSolverRequest = {
  runId: string;
  problemName: string;
  paramValues: Record<string, number>;
  arithmetic: Arithmetic;
  useSimplex: boolean;
  giveUpAccuracy: number;
  numberOfThreads: number;
};

export type SolverRunStatusUpdate = {
  runId: string;
  status: RunStatus;
};

export type RunStatus = 'RequestSent' | 'SolverRunning' | 'SolverFinished';

// getting solver steps

export type GetStepsRequest = {
  runId: string;
};

export type StepsResponse = {
  runId: string;
  steps: Step[];
  boxes: Record<BoxHash, Box>;
};

//////////////////////////////
// Overall request and response types

export type ProverRequest =
  | { tag: 'RequestGetExampleProblems'; contents: GetExampleProblemsRequest }
  | { tag: 'RequestGetAllFormulaNodes'; contents: GetAllFormulaNodesRequest }
  | { tag: 'RequestRunSolver'; contents: RunSolverRequest }
  | { tag: 'RequestGetSteps'; contents: GetStepsRequest };

export type ProverResponse =
  | { tag: 'ResponseExampleProblems'; contents: ExampleProblemsResponse }
  | { tag: 'ResponseFormulaNodes'; contents: FormulaNodesResponse }
  | { tag: 'ResponseSolverRunStatusUpdate'; contents: SolverRunStatusUpdate }
  | { tag: 'ResponseSteps'; contents: StepsResponse };

export function sendProverRequest(ws: Websocket, request: ProverRequest) {
  ws.send(JSON.stringify(request));
}
