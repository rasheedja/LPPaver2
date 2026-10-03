# Changelog

## Unreleased

- Default an omitted `useSimplex` solver request field to `false` for older clients, while preserving explicit values and rejecting invalid types.
- Pin the patched AERN2 affine dependency to preserve independent trigonometric uncertainty and prevent AA evaluation and pruning from discarding feasible points.
