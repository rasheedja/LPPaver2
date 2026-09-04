import { defineStore } from 'pinia';
import { computed, reactive, readonly, ref, watch, type DeepReadonly, type Ref } from 'vue';
import _ from 'lodash';
import { getProverWS } from './proverWS';
import type { Box, BoxHash } from '@/boxes/boxes';
import { exprHashToExpr, type Expr, type ExprF, type ExprHash } from '@/formulas/exprs';
import { formHashToForm, type Form, type FormF, type FormHash } from '@/formulas/forms';
import {
  type ProblemWithParamSpec,
  type Arithmetic,
  type RunStatus,
  type ProverResponse,
  type ProverRequest,
  sendProverRequest,
} from './proverMessage';
import type { Step } from '@/steps/steps';

export type RunInfo = {
  runId: string;
  problemName: string;
  paramValues: Record<string, number>;
  status: RunStatus;
  steps: Step[];
};

export const useProverStore = defineStore('prover', () => {
  const exampleProblemsList: Ref<Array<[string, ProblemWithParamSpec]>> = ref([]);
  const boxes: Ref<Record<BoxHash, Box>> = ref({});
  const exprs: Ref<Record<ExprHash, ExprF<ExprHash>>> = ref({});
  const forms: Ref<Record<FormHash, FormF<ExprHash, FormHash>>> = ref({});
  const runs: Ref<Record<string, RunInfo>> = ref({});
  const currentRunId: Ref<string | null> = ref(null);

  const exampleProblems = computed(() => Object.fromEntries(exampleProblemsList.value));

  const exports = {
    exampleProblemsList,
    boxes,
    exprs,
    forms,
    runs,
    currentRunId,
    getBox,
    getExpr,
    getForm,
    resetRunId,
    startRun,
    getRunInfo,
    exampleProblems,
  };

  function getExpr(exprHash: ExprHash): Expr {
    return exprHashToExpr(exprHash, exprs.value);
  }

  function getForm(formHash: FormHash): Form | undefined {
    return formHashToForm(formHash, forms.value, exprs.value);
  }

  function getBox(boxHash: BoxHash): Box {
    const box = boxes.value[boxHash];
    if (!box) {
      console.log(`boxes.value = `, boxes.value);
      console.log(`typeof(boxHash) = `, typeof boxHash);

      throw new Error(`Box with hash ${boxHash} not found`);
    }
    return box;
  }

  function resetRunId() {
    currentRunId.value = null;
  }

  async function startRun(
    problemName: string,
    paramValues: Record<string, number>,
    arithmetic: Arithmetic,
    useSimplex: boolean,
    giveUpAccuracy: number,
    numberOfThreads: number = 1,
  ) {
    const ws = await getProverWS();
    const runId = generateRunId();
    const message: ProverRequest = {
      tag: 'RequestRunSolver',
      contents: {
        runId,
        problemName,
        paramValues,
        arithmetic,
        useSimplex,
        giveUpAccuracy,
        numberOfThreads,
      },
    };

    sendProverRequest(ws, message);

    runs.value[runId] = reactive({
      runId,
      problemName,
      paramValues,
      status: 'RequestSent',
      steps: [], // TODO: try this out
    });

    currentRunId.value = runId;
  }

  function generateRunId(): string {
    // generate a random 12-character alphanumeric string
    return Math.random().toString(36).substring(2, 14);
  }

  function getRunInfo(runId: string): DeepReadonly<RunInfo> | null {
    const runInfo = runs.value[runId];
    if (!runInfo) return null;
    return readonly(runInfo);
  }

  //////////////////////////////////////////
  // Updating state based on prover messages
  //////////////////////////////////////////

  async function _watchProverMessages() {
    const ws = await getProverWS();
    ws.addEventListener('message', (ws, event) => {
      // console.log(`ws message event:`, event);

      const message: ProverResponse = JSON.parse(event.data);
      console.log(`ws message:`, message);
      switch (message.tag) {
        case 'ResponseExampleProblems': {
          exampleProblemsList.value = message.contents.problems;
          boxes.value = { ...boxes.value, ...message.contents.boxes };
          break;
        }
        case 'ResponseNewFormulaNodes': {
          exprs.value = { ...exprs.value, ...message.contents.exprs };
          forms.value = { ...forms.value, ...message.contents.forms };
          break;
        }
        case 'ResponseSolverRunStatusUpdate': {
          const { runId, status, newSteps, newBoxes } = message.contents;
          if (runs.value[runId]) {
            runs.value[runId].status = status;
            // absorb new steps and boxes
            if (!_.isEmpty(newSteps)) {
              runs.value[runId].steps = [...runs.value[runId].steps, ...newSteps];
            }
            if (!_.isEmpty(newBoxes)) {
              boxes.value = { ...boxes.value, ...newBoxes };
            }
          } else {
            console.warn(`Received run status for unknown runId ${runId}`);
          }
          break;
        }
        default:
          console.warn('Unrecognised message from prover backend:', message);
      }
    });
  }

  // start watching for messages from the prover backend
  _watchProverMessages();

  /////////////////////////
  // initialise the store
  /////////////////////////

  let listeningToFormulaFragments = false;

  // when exampleProblems is first assigned, request the formula fragments stream
  watch(exampleProblems, async () => {
    // do this only once when exampleProblems is first assigned
    if (_.isEmpty(exampleProblems.value)) return;
    if (listeningToFormulaFragments) return;
    listeningToFormulaFragments = true;

    const ws = await getProverWS();
    const message: ProverRequest = {
      tag: 'RequestKeepGettingFormulaNodes',
      contents: [],
    };
    sendProverRequest(ws, message);
  });

  // request example problems on store initialisation
  requestExampleProblems();

  async function requestExampleProblems() {
    const ws = await getProverWS();
    const message: ProverRequest = {
      tag: 'RequestGetExampleProblems',
      contents: [],
    };
    sendProverRequest(ws, message);
  }

  return exports;
});
