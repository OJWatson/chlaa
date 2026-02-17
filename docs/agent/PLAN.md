# Agent Plan and Status

Last updated: 2026-02-17

## Baseline gates

- `make check-sync`: PASS (generated artefacts match `inst/odin/generation_manifest.csv`)
- `R CMD build .`: PASS (`chlaa_0.1.0.tar.gz` built locally)
- `R CMD check --as-cran chlaa_0.1.0.tar.gz` (Linux local): FAIL in this environment due missing `pdflatex`; code/tests pass

## Current warnings/notes summary

- `R CMD check --as-cran` warnings fixed in repo code:
  - undeclared imports (`ggplot2`, `monty`, `tibble`) now declared
  - unused imports (`plyr`, `squire`) removed
- Remaining local-only issue:
  - manual build requires LaTeX (`pdflatex`) in this machine
- Remaining expected top-level NOTE risk if CRAN-like checks run with non-standard files:
  - `SCHEMA.md`, `spec.txt`, `chlaa_spec.md`, `Makefile`

## Drift summary

- Generator drift: clean (`check-sync` PASS)
- Bundled generators present:
  - `inst/odin/cholera_model.R`
  - `inst/odin/cholera_model_fit.R`
- Manifest includes source and generated artefacts:
  - `inst/odin/generation_manifest.csv`

## Now / Next / Later

- Now:
  - Stabilise CI as authoritative gate:
    - run `make check-sync` before package check
    - add no-binaries guard
    - fail CI on warnings
  - Completed on 2026-02-17.
- Next:
  - Execute scientific-fidelity milestone work:
    - add model mapping document
    - add parameter completeness + invariant tests
  - Completed (foundation level) on 2026-02-17.
- Later:
  - Expand fit workflow ergonomics + PPC outputs
  - Expand economics/optimisation summaries and tests
  - Completed on 2026-02-17.

## Decisions needed

1. CI strictness:
   - current workflow uses `R CMD check --no-manual` with `error-on: warning` to avoid host LaTeX variability while keeping strict warning gates.
2. Spec files:
   - keep `spec.txt` and `chlaa_spec.md` as project planning inputs, or move to `docs/` to avoid top-level CRAN NOTE noise.
