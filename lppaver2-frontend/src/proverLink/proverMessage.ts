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

export type KeepGettingFormulaNodesRequest = [];

export type NewFormulaNodesResponse = {
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
  giveUpAccuracy: number;
  numberOfThreads: number;
};

export type SolverRunStatusUpdate = {
  runId: string;
  status: RunStatus;
  newSteps: Step[];
  newBoxes: Record<BoxHash, Box>;
};

export type RunStatus = 'RequestSent' | 'SolverRunning' | 'SolverFinished';

//////////////////////////////
// Overall request and response types

export type ProverRequest =
  | { tag: 'RequestGetExampleProblems'; contents: GetExampleProblemsRequest }
  | { tag: 'RequestKeepGettingFormulaNodes'; contents: KeepGettingFormulaNodesRequest }
  | { tag: 'RequestRunSolver'; contents: RunSolverRequest };

export type ProverResponse =
  | { tag: 'ResponseExampleProblems'; contents: ExampleProblemsResponse }
  | { tag: 'ResponseNewFormulaNodes'; contents: NewFormulaNodesResponse }
  | { tag: 'ResponseSolverRunStatusUpdate'; contents: SolverRunStatusUpdate };

export function sendProverRequest(ws: Websocket, request: ProverRequest) {
  ws.send(JSON.stringify(request));
}
