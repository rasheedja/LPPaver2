<script lang="ts" setup>
  import { computed, onMounted, onUnmounted, ref, watch } from 'vue';
  import { storeToRefs } from 'pinia';
  import Plotly from 'plotly.js-dist-min';

  import { getSubProblems, type Step } from '@/steps/steps';
  import { useProverStore } from '@/proverLink/proverStore';
  import { useStepsStore } from '@/steps/stepsStore';
  import { intervalWidth, type Interval } from '@/formulas/evalInfo';
  import type { Kleenean } from '@/formulas/kleenean';
  import type { Problem } from '@/problems/problems';
  import { pickXY, type BoxHash, type Var } from '@/boxes/boxes';
  import { getTruthColour } from '@/styling';

  const props = withDefaults(
    defineProps<{
      topProblem: Problem | null;
    }>(),
    {},
  );

  const stepsStore = useStepsStore();
  const proverStore = useProverStore();
  const { focusedProblem, steps } = storeToRefs(stepsStore);

  const plotDiv = ref<Plotly.PlotlyHTMLElement | null>(null);

  const topScopeH = computed(() => props.topProblem?.scope ?? null);
  const topScopeBox = computed(() =>
    !topScopeH.value ? null : proverStore.getBox(topScopeH.value),
  );
  // const topScopeVarDomains = computed(() => topScopeBox.value?.varDomains ?? {});
  const topScopeVars = computed(() => topScopeBox.value?.box_.splitOrder ?? []);

  const xVar = ref<Var>('_x');
  const yVar = ref<Var>('_y');

  watch(
    topScopeBox,
    (newBox) => {
      if (!newBox) {
        return;
      }
      const xyVars = pickXY(newBox);
      xVar.value = xyVars.xVar;
      yVar.value = xyVars.yVar;
    },
    { immediate: true },
  );

  const topXDomain = computed(() => topScopeBox.value?.box_.varDomains[xVar.value]);
  const topYDomain = computed(() => topScopeBox.value?.box_.varDomains[yVar.value]);

  const xMaxSizeToShow = computed(() =>
    topXDomain.value ? intervalWidth(topXDomain.value) / 200 : 0,
  );
  const yMaxSizeToShow = computed(() =>
    topYDomain.value ? intervalWidth(topYDomain.value) / 200 : 0,
  );

  function getProblemTraces(problem: Problem | null): Partial<Plotly.Data>[] {
    if (!problem) {
      return [];
    }

    const step: Step | null = stepsStore.stepFromProblem(problem);

    // is this a progress step with inner / outer regions?
    const innerRegionH =
      step?.tag === 'ProgressStep' ? step.progressPaving.inner.boxes[0] : undefined;
    const outerRegionH =
      step?.tag === 'ProgressStep' ? step.progressPaving.outer.boxes[0] : undefined;

    // get the main step box (possibly with an excluded region)
    // and the Kleenean truth value for that box
    const [stepBoxH, stepTruth]: [BoxHash, Kleenean] = innerRegionH
      ? [innerRegionH, 'CertainTrue']
      : outerRegionH
        ? [outerRegionH, 'CertainFalse']
        : [problem.scope, 'TrueOrFalse'];

    // get this problem's scope shape
    const box = proverStore.getBox(stepBoxH);
    const varDomains = box.box_.varDomains;
    const xDomain = varDomains[xVar.value];
    const yDomain = varDomains[yVar.value];
    if (!xDomain || !yDomain) return [];
    const excludedRegion = box.box_.except;
    const excludedXDomain = excludedRegion ? excludedRegion[xVar.value] : undefined;
    const excludedYDomain = excludedRegion ? excludedRegion[yVar.value] : undefined;
    const { x, y } = getBoxTraceXY(xDomain, yDomain, excludedXDomain, excludedYDomain);

    const problemLineTrace: Partial<Plotly.Data> = {
      type: 'scatter',
      x,
      y,
      mode: 'lines',
      fill: 'toself',
      line: {
        color: 'black',
        width: 1,
      },
      fillcolor: getTruthColour(stepTruth),
      opacity: 0.5,
      showlegend: false,
      customdata: [problem.scope, problem.constraint],
      hoverinfo: 'none',
    };

    // check whether to skip rendering subproblems
    const isTooSmallToSubdivide =
      intervalWidth(xDomain) < xMaxSizeToShow.value
      && intervalWidth(yDomain) < yMaxSizeToShow.value;

    if (isTooSmallToSubdivide) {
      return [problemLineTrace];
    }

    // recursively get subproblem shapes
    const subProblemTraces: Partial<Plotly.Data>[] = getSubProblems(step).flatMap(getProblemTraces);

    return [problemLineTrace, ...subProblemTraces];
  }

  function getBoxTraceXY(
    xDomain: Interval<number> | undefined,
    yDomain: Interval<number> | undefined,
    excludedXDomain: Interval<number> | undefined,
    excludedYDomain: Interval<number> | undefined,
  ) {
    const x0 = xDomain?.l ?? 0;
    const x1 = xDomain?.u ?? 0;
    const y0 = yDomain?.l ?? 0;
    const y1 = yDomain?.u ?? 0;

    const x0Excl = excludedXDomain?.l ?? 0;
    const x1Excl = excludedXDomain?.u ?? 0;
    const y0Excl = excludedYDomain?.l ?? 0;
    const y1Excl = excludedYDomain?.u ?? 0;

    if (excludedXDomain && excludedYDomain) {
      return {
        x: [x0, x1, x1, x0, x0, x0Excl, x0Excl, x1Excl, x1Excl, x0Excl],
        y: [y0, y0, y1, y1, y0, y0Excl, y1Excl, y1Excl, y0Excl, y0Excl],
      };
    } else {
      return {
        x: [x0, x1, x1, x0, x0],
        y: [y0, y0, y1, y1, y0],
      };
    }
  }

  function getFocusedProblemOutline() {
    if (!focusedProblem.value) {
      return [];
    }

    const box = proverStore.getBox(focusedProblem.value.scope);
    const varDomains = box.box_.varDomains;
    const outline: Partial<Plotly.Shape> = {
      type: 'rect',
      x0: varDomains[xVar.value]?.l ?? 0,
      x1: varDomains[xVar.value]?.u ?? 0,
      y0: varDomains[yVar.value]?.l ?? 0,
      y1: varDomains[yVar.value]?.u ?? 0,
      line: {
        color: 'red',
        width: 3,
      },
      fillcolor: 'rgba(0,0,0,0)', // transparent fill
      showlegend: false,
    };

    return [outline];
  }

  function getAxisLayout(v: Var): Partial<Plotly.LayoutAxis> {
    return {
      // title: { text: v },
      zeroline: false,
      showgrid: false,
      automargin: true,
    };
  }

  const layout = computed<Partial<Plotly.Layout>>(() => ({
    autosize: true,
    uirevision: -1,
    xaxis: { ...getAxisLayout(xVar.value) },
    yaxis: { ...getAxisLayout(yVar.value) },
    margin: { t: 5, b: 5, l: 40, r: 5 },
    shapes: [...getFocusedProblemOutline()],
    // dragmode: "pan",
    dragmode: 'zoom',
  }));

  let handlerAttached = false;

  function renderPlot() {
    if (!plotDiv.value) {
      return;
    }

    Plotly.react(plotDiv.value, getProblemTraces(props.topProblem), layout.value, {
      displayModeBar: true,
      responsive: true,
      scrollZoom: true,
    });

    // add a Plotly mouse click event handler
    if (plotDiv.value && !handlerAttached) {
      handlerAttached = true;
      plotDiv.value.on('plotly_click', (data) => {
        // console.log('Plotly click event data:', data);
        const point = data.points[0];
        const scope = point?.data.customdata[0] as string | undefined;
        const constraint = point?.data.customdata[1] as string | undefined;
        if (scope && constraint) {
          focusedProblem.value = { scope, constraint };
        }
      });
    }
  }

  watch([plotDiv, steps, () => props.topProblem, xVar, yVar, focusedProblem], renderPlot);

  onMounted(renderPlot);
  onUnmounted(() => {
    if (plotDiv.value) {
      Plotly.purge(plotDiv.value);
    }
  });
</script>

<template>
  <!-- TODO: make plot resize with grid cell resize -->
  <div ref="plotDiv" class="w-100" style="height: 90%"></div>
  <div class="d-flex p-2 align-items-center border-top">
    <span class="mx-2">Y variable:</span>
    <select class="form-select" style="width: min-content" v-model="yVar">
      <option v-for="v in topScopeVars" :key="v" :value="v">{{ v }}</option>
    </select>

    <span class="ms-auto me-2">X variable:</span>
    <select class="form-select me-auto" style="width: min-content" v-model="xVar">
      <option v-for="v in topScopeVars" :key="v" :value="v">{{ v }}</option>
    </select>
  </div>
</template>
