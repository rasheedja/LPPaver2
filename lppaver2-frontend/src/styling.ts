import type { Kleenean } from './formulas/kleenean';
import { getStepTruthResult, type Step } from './steps/steps';

export function getTruthColour(kleenean: Kleenean): string {
  switch (kleenean) {
    case 'CertainTrue':
      return '#e0ffe0';
    case 'CertainFalse':
      return '#ffd0e0';
    case 'TrueOrFalse':
      return '#e0e0ff';
  }
}

export function getStepColour(step: Step | null) {
  if (!step) return '#ffffff';

  const truthResult = getStepTruthResult(step);
  return getTruthColour(truthResult);
}
