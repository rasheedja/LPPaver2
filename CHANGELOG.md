# Changelog

## Unreleased

- Leave parameter domains unchanged during bound application while continuing to detect contradictory parameter bounds.
- Reuse the filtered relaxation coefficients when constructing simplex constraints and remove an unreachable empty-map check.
- Scope the simplex-method dependency to the library component that imports it.
- Pin the AERN2 affine package to the reciprocal fix in `rasheedja/aern2` at `6eac549`, retaining upstream trigonometric and reciprocal fallback fixes while preserving independent reciprocal intercept uncertainty.
- Update simplex-method to the latest merged fixes and cleanups at `590c40f`.
- Test that square-root simplex pruning tightens strictly positive domains while retaining generated feasible boundary points.
- Reuse `isCertainlyNonZero` when guarding interval reciprocals instead of maintaining a duplicate endpoint predicate.
- Use the existing exact `rational` conversion for affine coefficients instead of a local MPFloat conversion wrapper.
- Remove the duplicate `simplexPruneWithEvalValues` entry point; use `simplexPrune` with its existing evaluated-expression argument.
