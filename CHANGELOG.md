# Changelog

## Unreleased

- Collect formula variables by visiting each reachable formula and expression hash once, avoiding repeated expansion of shared DAGs.
- Share affine source-variable preparation across comparisons within each ordinary or simplex pruning call.
- Skip simplex objectives for volume variables absent from the extracted constraints, preserving their domains and feasibility checks.
- Leave parameter domains unchanged during bound application while continuing to detect contradictory parameter bounds.
- Reuse the filtered relaxation coefficients when constructing simplex constraints and remove an unreachable empty-map check.
- Scope the simplex-method dependency to the library component that imports it.
- Pin the AERN2 affine package to upstream `michalkonecny/aern2` at `c507b29`, where the reciprocal intercept provenance fix (`6eac549`) is integrated, instead of the temporary `rasheedja/aern2` fork. This retains upstream trigonometric and reciprocal fallback fixes while preserving independent reciprocal intercept uncertainty.
- Test that ordinary and simplex pruning retain feasible points for independent reciprocal expressions with matching ranges, in both interval and affine arithmetic.
- Pin simplex-method to `9d59ab1` for Bland's anti-cycling rule and removal of artificial basic variables before phase two, retaining the earlier merged fixes and cleanups.
- Test that square-root simplex pruning tightens strictly positive domains while retaining generated feasible boundary points.
- Reuse `isCertainlyNonZero` when guarding interval reciprocals instead of maintaining a duplicate endpoint predicate.
- Use the existing exact `rational` conversion for affine coefficients instead of a local MPFloat conversion wrapper.
- Remove the duplicate `simplexPruneWithEvalValues` entry point; use `simplexPrune` with its existing evaluated-expression argument.
