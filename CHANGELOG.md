# Changelog

## Unreleased

- Remove the duplicate `simplexPruneWithEvalValues` entry point; use `simplexPrune` with its existing evaluated-expression argument.
- Pin the patched AERN2 affine dependency to preserve independent trigonometric uncertainty and prevent AA evaluation and pruning from discarding feasible points.
