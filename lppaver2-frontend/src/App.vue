<script setup lang="ts">
  import { computed, reactive } from 'vue';
  import { storeToRefs } from 'pinia';
  import { GridLayout, GridItem, type LayoutItem } from 'grid-layout-plus';
  import { useStepsStore } from './steps/stepsStore';
  import { useProverStore } from './proverLink/proverStore.ts';
  import { type FormOrExprHash } from './formulas/forms';
  import Paving2D from './paving2D/Paving2D.vue';
  import StepTree from './steps/StepTree.vue';
  import ProblemView from './ProblemView.vue';
  import FormExprPlot from './formulaExpPlot/FormExprPlot.vue';
  import Params from './Params.vue';

  const stepStore = useStepsStore();
  const proverStore = useProverStore();
  const { focusedProblem, focusedProblemSubFormExpr, focusedExprValues } = storeToRefs(stepStore);

  const focusedScopeH = computed(() => focusedProblem.value?.scope ?? null);
  const focusedScopeBox = computed(() =>
    !focusedScopeH.value ? null : proverStore.getBox(focusedScopeH.value),
  );

  const viewHeight = computed(() => window.innerHeight - 100);

  const paramsLayout = reactive<LayoutItem>({
    i: 'params',
    x: 0,
    y: 0,
    w: 12,
    h: 1,
  });
  const stepTreeLayout = reactive<LayoutItem>({
    i: 'stepTree',
    x: 0,
    y: 1,
    w: 6,
    h: 3,
  });
  const paving2DLayout = reactive<LayoutItem>({
    i: 'paving2D',
    x: 0,
    y: 4,
    w: 6,
    h: 4,
  });
  const focusedPLayout = reactive<LayoutItem>({
    i: 'focusedP',
    x: 6,
    y: 1,
    w: 6,
    h: 3,
  });
  const focEPlotLayout = reactive<LayoutItem>({
    i: 'focEPlot',
    x: 6,
    y: 4,
    w: 6,
    h: 4,
  });

  const layout = reactive<LayoutItem[]>([
    paramsLayout,
    stepTreeLayout,
    paving2DLayout,
    focusedPLayout,
    focEPlotLayout,
  ]);

  function onSubFormExprClicked(data: FormOrExprHash) {
    focusedProblemSubFormExpr.value = data;
  }
</script>

<template>
  <GridLayout
    v-model:layout="layout"
    :colNum="12"
    :rowHeight="(viewHeight - 40) / 8"
    :isDraggable="true"
    :isResizable="true"
    :responsive="false"
    :useCssTransforms="true"
  >
    <GridItem
      key="params"
      i="params"
      :x="paramsLayout.x"
      :y="paramsLayout.y"
      :w="paramsLayout.w"
      :h="paramsLayout.h"
      :isDraggable="false"
    >
      <div class="border w-100 h-100" style="overflow-y: auto"><Params /></div>
    </GridItem>
    <GridItem
      key="stepTree"
      i="stepTree"
      :x="stepTreeLayout.x"
      :y="stepTreeLayout.y"
      :w="stepTreeLayout.w"
      :h="stepTreeLayout.h"
      :isDraggable="false"
    >
      <div class="border w-100 h-100" style="overflow-y: auto">
        <StepTree />
      </div>
    </GridItem>
    <GridItem
      key="paving2D"
      i="paving2D"
      :x="paving2DLayout.x"
      :y="paving2DLayout.y"
      :w="paving2DLayout.w"
      :h="paving2DLayout.h"
      :isDraggable="false"
    >
      <div class="border w-100 h-100" style="overflow-y: auto">
        <Paving2D :topProblem="stepStore.zoomedProblem" />
      </div>
    </GridItem>
    <GridItem
      key="focusedP"
      i="focusedP"
      :x="focusedPLayout.x"
      :y="focusedPLayout.y"
      :w="focusedPLayout.w"
      :h="focusedPLayout.h"
      :isDraggable="false"
    >
      <ProblemView
        :problem="focusedProblem"
        @click="onSubFormExprClicked"
        :highlightedSubFormExpr="focusedProblemSubFormExpr ?? undefined"
      />
    </GridItem>
    <GridItem
      key="focEPlot"
      i="focEPlot"
      :x="focEPlotLayout.x"
      :y="focEPlotLayout.y"
      :w="focEPlotLayout.w"
      :h="focEPlotLayout.h"
      :isDraggable="false"
    >
      <div class="border w-100 h-100" style="overflow-y: auto">
        <FormExprPlot
          v-if="focusedProblemSubFormExpr && focusedExprValues"
          :formOrExprHash="focusedProblemSubFormExpr"
          :box="focusedScopeBox ?? undefined"
          :exprValues="focusedExprValues"
        />
      </div>
    </GridItem>
  </GridLayout>
</template>
