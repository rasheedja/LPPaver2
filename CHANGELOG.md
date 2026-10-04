# Changelog

## Unreleased

- Test that square-root simplex pruning tightens strictly positive domains while retaining generated feasible boundary points.
- Reuse `isCertainlyNonZero` when guarding interval reciprocals instead of maintaining a duplicate endpoint predicate.
- Use the existing exact `rational` conversion for affine coefficients instead of a local MPFloat conversion wrapper.
- Remove the duplicate `simplexPruneWithEvalValues` entry point; use `simplexPrune` with its existing evaluated-expression argument.
- Pin the patched AERN2 affine dependency to preserve independent trigonometric uncertainty and prevent AA evaluation and pruning from discarding feasible points.
