# Milestones

This file is a lightweight marker of milestone boundaries in the task graph.

## M0 — Packaging correctness and build hygiene

Completed: 2026-02-15

Scope (see `rich/taskgraphs/chlaa.yaml` / `rich/specs/chlaa_spec.md`):
- No compiled artefacts are tracked or shipped (`*.o`, `*.so`, `*.dll`, `*.dylib`).
- Bundled generators are present and documented:
  - `inst/odin/cholera_model.R` (simulation)
  - `inst/odin/cholera_model_fit.R` (fit/likelihood)
- Regeneration workflow is explicit (`tools/regenerate_model.R`) and drift is detectable.
- Documentation updated to match the bundled-generator approach.

Notes:
- Local `R CMD check --as-cran` may still emit CRAN-oriented WARNING/NOTE about non-CRAN deps and non-standard top-level files; CI is the gate for install + check success across platforms.
