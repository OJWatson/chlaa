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

## M1 — Bundled generator hardening

Status: Completed (2026-02-17)

Completed scope:
- Runtime sync checks added and passing (`make check-sync`).
- Tracked compiled artefact guard added (`make check-no-binaries`, `tools/check_no_binaries.sh`).
- Core package tests include simulation + fit smoke coverage.
- Runtime odin2 compilation is now explicit opt-in via:
  - `options(chlaa.allow_runtime_odin2 = TRUE)` or
  - `CHLAA_ALLOW_RUNTIME_ODIN2=true`
- Added tests for compile gate behaviour.

## M2 — CI authoritative gates

Status: Completed (2026-02-17)

Completed scope:
- CI runs `make check-sync` before package checks.
- CI has a dedicated no-binaries job.
- CI package checks configured to fail on warnings (`error-on: warning`).

Remaining scope:
- Monitor first full matrix run and address platform-specific warnings if any.

## M3 — Scientific fidelity foundations

Status: Completed (2026-02-17)

Completed scope:
- Added `docs/model_mapping.md` linking conceptual stocks/flows/interventions to odin states and controls.
- Added generator/metadata completeness tests:
  - no orphan metadata parameters
  - all generator parameters present in `cholera_parameter_info()`
- Added simulation invariants tests:
  - non-negativity of core compartments
  - conservation of total population (including death counters)
  - incidence/cumulative counter consistency

Remaining scope:
- Expand parameter metadata `source` fields to explicit publication table/section citations.

## M4 — Fit workflow hardening

Status: Completed (2026-02-17)

Completed scope:
- Added `cholera_prepare_data()` with validation and optional regular-step padding.
- Added fit diagnostics/report helpers:
  - `cholera_fit_trace()`
  - `cholera_plot_trace()`
  - `cholera_fit_report()`
- `cholera_fit_pmcmc()` now uses validated prepared data.
- Added fit workflow tests (`tests/testthat/test-fit-workflow.R`).

## M5 — Scenario workflow outputs

Status: Completed (2026-02-17)

Completed scope:
- Added standard operational templates via `cholera_standard_scenarios()`.
- Added scenario summary helper `cholera_scenario_summary()`.
- Added high-level reporting helper `cholera_scenario_report()`.
- Added scenario workflow tests (`tests/testthat/test-scenario-summary.R`).

## M6 — Health economics and optimisation

Status: Completed (2026-02-17)

Completed scope:
- Added external economics defaults in `inst/extdata/econ/` and loader `cholera_econ_defaults()`.
- Added CEAC helper `cholera_ceac()`.
- Added NMB/INMB outputs in `cholera_compare_scenarios(..., wtp = )`.
- Upgraded `cholera_optimise_budget()` with allocation constraints and method choice.
- Added economics/optimisation tests (`tests/testthat/test-econ-optimise.R`).
- Added milestone vignettes:
  - `vignettes/fitting_workflow.Rmd`
  - `vignettes/scenario_workflow.Rmd`
  - `vignettes/health_econ_and_optimisation.Rmd`
