# Changelog

## Unreleased

- Reuse the filtered relaxation coefficients when constructing simplex constraints and remove an unreachable empty-map check.
- Scope the simplex-method dependency to the library component that imports it.
- Use upstream AERN2 commit `08fd51c` to preserve independent trigonometric uncertainty and prevent AA evaluation and pruning from discarding feasible points.
- Update simplex-method to the latest merged fixes and cleanups at `590c40f`.
- Test that square-root simplex pruning tightens strictly positive domains while retaining generated feasible boundary points.
- Reuse `isCertainlyNonZero` when guarding interval reciprocals instead of maintaining a duplicate endpoint predicate.
- Use the existing exact `rational` conversion for affine coefficients instead of a local MPFloat conversion wrapper.
- Remove the duplicate `simplexPruneWithEvalValues` entry point; use `simplexPrune` with its existing evaluated-expression argument.
