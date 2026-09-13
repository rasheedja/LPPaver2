<script lang="ts" setup>
  import { storeToRefs } from 'pinia';
  import { computed, ref, watch } from 'vue';
  import { useStepsStore } from './stepsStore';
  import { getStepTruthResult, getSubProblems } from './steps';
  import { sameProblem, type Problem } from '@/problems/problems';
  import { getStepColour } from '@/styling';

  const props = defineProps<{
    problem: Problem;
  }>();

  const stepsStore = useStepsStore();

  const { focusedProblem, zoomedProblem } = storeToRefs(stepsStore);

  const step = computed(() => stepsStore.stepFromProblem(props.problem));

  const stepLabel = computed(() => {
    if (step.value === null) {
      return 'No Step';
    }
    const stepTag = step.value.tag;
    const progressPaving = stepTag === 'ProgressStep' ? step.value.progressPaving : undefined;

    const stepCategory = //
      !progressPaving
        ? stepTag
        : progressPaving.undecided.length == 0
          ? 'Decided'
          : progressPaving.inner.boxes.length > 0
            ? 'Prune True'
            : progressPaving.outer.boxes.length > 0
              ? 'Prune False'
              : 'Split';

    const stepTruth = getStepTruthResult(step.value);

    const stepTruthNote =
      stepTruth === 'CertainTrue' ? ' (True)' : stepTruth === 'CertainFalse' ? ' (False)' : '';

    return `${stepCategory}${stepTruthNote}`;
  });

  const subProblems = computed(() => getSubProblems(step.value));

  const isFocused = computed(() => sameProblem(props.problem, focusedProblem.value));
  const isZoomed = computed(() => sameProblem(props.problem, zoomedProblem.value));

  const classes = computed(() => {
    return {
      normal: !isFocused.value && !isZoomed.value,
      focused: isFocused.value && !isZoomed.value,
      zoomed: isZoomed.value && !isFocused.value,
      focusedAndZoomed: isFocused.value && isZoomed.value,
    };
  });

  // Focus this problem when clicked
  function focusHere(event: MouseEvent) {
    focusedProblem.value = props.problem;
    // prevent event bubbling
    event.stopPropagation();
  }

  // Zoom this problem when double-clicked
  function zoomHere(event: MouseEvent) {
    zoomedProblem.value = props.problem;
    // prevent event bubbling
    event.stopPropagation();
  }

  const el = ref<HTMLElement | null>(null);

  // Scroll to here when focused
  watch(focusedProblem, (newVal) => {
    if (sameProblem(newVal, props.problem)) {
      // Scroll this element into view
      el.value?.scrollIntoView({
        behavior: 'smooth',
        block: 'start',
        inline: 'center',
      });
    }
  });
</script>

<template>
  <table
    ref="el"
    :class="classes"
    :style="`background-color: ${getStepColour(step)};`"
    @click="focusHere"
    @dblclick="zoomHere"
  >
    <tbody>
      <tr>
        <td colspan="2" class="text-left fw-bold">
          {{ stepLabel }}
        </td>
      </tr>
      <tr v-for="subProblem in subProblems">
        <td class="text-center" style="width: 15px; vertical-align: top; color: midnightblue">↳</td>
        <td>
          <StepTreeNode :problem="subProblem" />
        </td>
      </tr>
    </tbody>
  </table>
</template>
