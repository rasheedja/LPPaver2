import { defineStore, storeToRefs } from 'pinia';
import { computed, ref, watch, type Ref } from 'vue';
import type { ExprValue } from '@/formulas/evalInfo';
import { problemToProblemHash, type Problem, type ProblemHash } from '@/problems/problems';
import { type ExprHash } from '../formulas/exprs';
import { type FormOrExprHash } from '../formulas/forms';
import { getStepProblem, type Step } from './steps';
import { useProverStore } from '@/proverLink/proverStore';
import { getBoxVolume } from '@/boxes/boxes';

export type ProverStateStats = {
  percentInner: number;
  percentOuter: number;
  percentGivenUp: number;
  percentTodo: number;
};

export const useStepsStore = defineStore('steps', () => {
  const steps: Ref<Step[]> = ref([]);
  const numberOfSteps = ref(0); // keep steps separately to make it easier to define reactive dependencies
  const _problem2step: Ref<Record<ProblemHash, Step>> = ref({});
  const rootProblem: Ref<Problem | null> = ref(null);
  const focusedProblem: Ref<Problem | null> = ref(null);
  const focusedProblemSubFormExpr: Ref<FormOrExprHash | null> = ref(null); // set in App.vue when user clicks on a sub-form or sub-expr
  const zoomedProblem: Ref<Problem | null> = ref(null);

  const focusedExprValues = computed<Record<ExprHash, ExprValue> | undefined>(() => {
    if (!focusedProblem.value) return undefined;
    const step = _problem2step.value[problemToProblemHash(focusedProblem.value)];
    if (!step || step.tag !== 'ProgressStep') return undefined;
    return step.evalInfo.exprValues;
  });

  const stepsStats = computed<ProverStateStats | undefined>(() => {
    if (!rootProblem.value) return undefined;

    // compute the total volume of the root problem's box
    const proverStore = useProverStore();
    const rootBox = proverStore.getBox(rootProblem.value.scope);
    const totalVolume = getBoxVolume(rootBox);

    // compute the volumes of the boxes in the steps
    let volumeInner = 0;
    let volumeOuter = 0;
    let volumeGivenUp = 0;
    steps.value.forEach((step) => {
      if (step.tag === 'ProgressStep') {
        // progress steps
        step.progressPaving.inner.boxes.forEach((boxH) => {
          const box = proverStore.getBox(boxH);
          volumeInner += getBoxVolume(box);
        });
        step.progressPaving.outer.boxes.forEach((boxH) => {
          const box = proverStore.getBox(boxH);
          volumeOuter += getBoxVolume(box);
        });
      } else if (step.tag === 'GiveUpOnProblemStep') {
        volumeGivenUp += getBoxVolume(proverStore.getBox(step.problem.scope));
      }
    });

    const volumeTodo = totalVolume - (volumeInner + volumeOuter + volumeGivenUp);

    return {
      percentInner: (100 * volumeInner) / totalVolume,
      percentOuter: (100 * volumeOuter) / totalVolume,
      percentGivenUp: (100 * volumeGivenUp) / totalVolume,
      percentTodo: (100 * volumeTodo) / totalVolume,
    };
  });

  const exports = {
    steps,
    numberOfSteps,
    _problem2step,
    rootProblem,
    focusedProblem,
    focusedProblemSubFormExpr,
    zoomedProblem,
    previewProblem,
    setInitProblem,
    resetSteps,
    stepFromProblem,
    focusedExprValues,
    stepsStats,
  };

  async function resetSteps() {
    steps.value = [];
    numberOfSteps.value = 0;
    _problem2step.value = {};
    setInitProblem(null);
  }

  async function setInitProblem(initProblem: Problem | null) {
    rootProblem.value = initProblem;
    zoomedProblem.value = initProblem;
    focusedProblem.value = initProblem;
    focusedProblemSubFormExpr.value = null;
  }

  async function previewProblem(problem: Problem) {
    resetSteps();
    setInitProblem(problem);
  }

  function stepFromProblem(p: Problem): Step | null {
    const problemHash = problemToProblemHash(p);
    const step = _problem2step.value[problemHash];
    if (!step) {
      return null;
    }
    return step;
  }

  // track the steps of the current run
  const proverStore = useProverStore();
  const { runs, currentRunId } = storeToRefs(proverStore);
  const currentRunSteps = computed(() => {
    const steps = runs.value[currentRunId.value ?? '']?.steps;
    if (!steps || steps.length == 0) return undefined;
    return steps;
  });

  watch(currentRunSteps, () => {
    if (!currentRunSteps.value) return;
    const stepsExceptInitDone = currentRunSteps.value.filter(
      (step: Step) => step.tag !== 'InitStep' && step.tag !== 'DoneStep',
    );
    steps.value = stepsExceptInitDone;
    numberOfSteps.value = stepsExceptInitDone.length;
    _problem2step.value = {};
    // build the problem2step mapping for all steps except InitStep
    for (const step of stepsExceptInitDone) {
      const problem = getStepProblem(step);
      if (problem) {
        const problemHash = problemToProblemHash(problem);
        _problem2step.value[problemHash] = step;
      }
    }
    // if not set, set the root problem to the problem of the first step (if it exists)
    if (rootProblem.value === null && stepsExceptInitDone.length > 0) {
      const firstStepProblem = getStepProblem(stepsExceptInitDone[0]!);
      if (firstStepProblem) {
        setInitProblem(firstStepProblem);
      }
    }
  });

  return exports;
});
