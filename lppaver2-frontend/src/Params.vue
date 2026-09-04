<script lang="ts" setup>
  import { computed, ref, watch, type DeepReadonly } from 'vue';
  import { storeToRefs } from 'pinia';
  import { useStepsStore } from './steps/stepsStore';
  import { useProverStore } from './proverLink/proverStore.ts';
  import type { Arithmetic, ParamSpec } from './proverLink/proverMessage.ts';
  import { getTruthColour } from './styling.ts';

  const stepsStore = useStepsStore();
  const { numberOfSteps, stepsStats } = storeToRefs(stepsStore);
  const proverStore = useProverStore();
  const { currentRunId } = storeToRefs(proverStore);

  const currentRunInfo = computed(() => {
    if (!currentRunId.value) return null;
    return proverStore.getRunInfo(currentRunId.value);
  });

  const currentRunStatus = computed(() => currentRunInfo.value?.status ?? null);

  const canStartRun = computed(
    () => selectedProblemName.value !== null && currentRunStatus.value !== 'SolverRunning',
  );

  const selectedProblemName = ref<string | null>(null);
  const selectedProblem = computed(() => {
    if (!selectedProblemName.value) {
      return null;
    }
    return proverStore.exampleProblems[selectedProblemName.value];
  });

  type ArithType = 'BallArithmetic' | 'AffineArithmetic';

  const selectedArithType = ref<ArithType>('BallArithmetic');
  const selectedPrecision = ref<number>(100);
  const selectedMaxTerms = ref<number>(10);
  const useSimplex = ref<boolean>(false);

  const selectedArithmetic = computed<Arithmetic>(() => {
    return selectedArithType.value === 'BallArithmetic'
      ? { tag: 'BallArithmetic', precision: selectedPrecision.value }
      : {
          tag: 'AffineArithmetic',
          precision: selectedPrecision.value,
          maxTerms: selectedMaxTerms.value,
        };
  });

  const giveUpAccuracy = ref<number>(0.01);

  type ParamValue = {
    spec: DeepReadonly<ParamSpec>;
    val: number;
  };

  const params = ref<ParamValue[]>([]);

  function run() {
    if (!selectedProblemName.value) return;

    // transform params array into a record of paramName -> val
    const paramsObj: Record<string, number> = {};
    for (const param of params.value) {
      paramsObj[param.spec.paramName] = param.val;
    }

    proverStore.startRun(
      selectedProblemName.value,
      paramsObj,
      selectedArithmetic.value,
      useSimplex.value,
      giveUpAccuracy.value,
    );
  }

  watch(selectedProblem, (newProblem) => {
    if (newProblem) {
      stepsStore.previewProblem(newProblem.problem);
      params.value = newProblem.paramSpecs.map((spec) => ({
        spec,
        val: spec.defaultValue,
      }));
    }
  });
</script>

<template>
  <div class="d-flex align-items-baseline gap-2">
    <!-- problem selector -->
    <div class="mb-2">
      <select class="form-select" v-model="selectedProblemName">
        <option :value="null">Select a problem</option>
        <option v-for="([name, p], i) in proverStore.exampleProblemsList" :key="name" :value="name">
          {{ name }}
        </option>
      </select>
    </div>
    <!-- parameter inputs -->
    <div v-if="params.length > 0">
      <div
        v-for="param in params"
        :key="param.spec.paramName"
        class="d-flex align-items-baseline gap-2"
      >
        <label :for="param.spec.paramName" class="form-label">{{ param.spec.paramName }}</label>
        <input
          type="number"
          class="form-control"
          :id="param.spec.paramName"
          v-model.number="param.val"
          :min="param.spec.minValue"
          :max="param.spec.maxValue"
          :step="undefined"
        />
      </div>
    </div>
    <div class="flex-grow-1">&nbsp;</div>
    <!-- Choice of arithmetic -->
    <select class="form-select w-auto" v-model="selectedArithType">
      <option value="BallArithmetic">MP Interval Arithmetic</option>
      <option value="AffineArithmetic">MP Affine Arithmetic</option>
    </select>
    <div class="form-check">
      <input
        class="form-check-input"
        type="checkbox"
        id="useSimplex"
        v-model="useSimplex"
      />
      <label class="form-check-label" for="useSimplex">Use simplex pruning</label>
    </div>
    <!-- Input max size of box before giving up -->
    <label for="giveUpAccuracy" class="form-label">Max box size: </label>
    <input
      type="number"
      class="form-control w-auto"
      id="giveUpAccuracy"
      v-model.number="giveUpAccuracy"
      step="0.01"
    />
    <!-- run button -->
    <button :disabled="!canStartRun" class="btn btn-primary" @click="run">Run</button>
  </div>
  <div class="d-flex align-items-baseline">
    <div class="mx-2">
      <span v-if="currentRunStatus === 'RequestSent'">Run in progress...</span>
      <span v-else-if="currentRunStatus === 'SolverRunning'">Run in progress...</span>
      <span v-else-if="currentRunStatus === 'SolverFinished'">Run finished</span>
    </div>
    <div v-if="stepsStats" class="d-flex align-items-baseline gap-2">
      <span class="p-1">{{ numberOfSteps }} step(s)</span>
      <span class="p-1" :style="{ backgroundColor: getTruthColour('CertainTrue') }"
        >Inner: {{ stepsStats.percentInner.toFixed(2) }}%
      </span>
      <span class="p-1" :style="{ backgroundColor: getTruthColour('CertainFalse') }"
        >Outer: {{ stepsStats.percentOuter.toFixed(2) }}%
      </span>
      <span class="p-1" :style="{ backgroundColor: getTruthColour('TrueOrFalse') }"
        >Unknown: {{ stepsStats.percentUnknown.toFixed(2) }}%
      </span>
    </div>
  </div>
</template>
