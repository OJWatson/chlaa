# Repository schema (chlaa)

This is a standalone R package implementing a stochastic cholera outbreak model in **odin2** (dust2 backend),
with utilities for fitting, counterfactual simulation, forecasting, and health economic evaluation.

End users should not need odin2 installed: the repository commits the generated C++ and bindings so simulation and fitting work from a clean checkout. odin2 is only required for developers who want to edit `inst/odin/*.R` and regenerate the bundled outputs (see `docs/packaging.md`).

## Directory layout

- `DESCRIPTION`, `NAMESPACE`, `LICENSE`: standard R package metadata
- `Makefile`: developer helper targets (`make regen`, `make check-sync`, `make check-no-binaries`)
- `R/`: package code (functions)
- `inst/odin/`: odin2 model files
  - `cholera_model.R`: simulation model
  - `cholera_model_fit.R`: adds an observation model for filtering / likelihood
- `man/`: prebuilt documentation (keeps R CMD check quiet without roxygen)
- `tests/`: unit tests (including smoke tests for simulation and fit paths)
- `tools/`: developer scripts for regeneration and drift checks
- `docs/`: project documentation (model mapping, packaging notes)
- `vignettes/`: end-to-end analyst workflows (fit, scenarios, economics/optimisation)
- `inst/extdata/econ/`: externalised economics defaults (unit costs and DALY parameters)
- `_pkgdown.yml`: pkgdown site configuration
- `.github/workflows/pkgdown.yaml`: docs build/deploy workflow

## Key files

### `inst/odin/`
- `cholera_model.R`
  - Discrete-time stochastic compartmental model
  - Daily incidence variables (`inc_*`) reset with `zero_every = 1`
  - Cumulative outputs (`cum_*`) for scenario evaluation and economics

- `cholera_model_fit.R`
  - Same as `cholera_model.R`
  - Adds data likelihood for `cases` (Negative Binomial) using `reporting_rate` and `obs_size`

### `R/`
- `parameters.R`
  - `chlaa_parameter_info()`: default values + metadata
  - `chlaa_parameters()`: build parameter list with overrides
  - `chlaa_parameters_validate()`: basic sanity checks

- `model.R`
  - `chlaa_simulate()`: run the model via dust2
  - `chlaa_generator()`: internal generator getter (packaged generator or runtime odin2 compile)

- `fit.R`
  - `chlaa_fit_pmcmc()`: particle MCMC fitting (monty + dust2)

- `fit_object.R`
  - `chlaa_fit` S3 helpers (`print`, `summary`, `chlaa_posterior_summary()`)

- `posterior.R`
  - draw extraction + parameter updating (`chlaa_update_from_fit()`)
  - posterior predictive simulations (`chlaa_simulate_posterior()`)

- `scenario_builder.R`
  - `chlaa_trigger_time_from_sim()`
  - `chlaa_make_aa_scenarios()`: AA scenario templates

- `scenario.R`
  - `chlaa_scenario()`, `chlaa_run_scenarios()`
  - `chlaa_compare_scenarios()` (optionally with economics)

- `counterfactual_grid.R`
  - `chlaa_counterfactual_grid()`: tidy grid of scenario knobs
  - `chlaa_run_counterfactual_grid()`

- `forecast.R`
  - `chlaa_forecast_from_fit()` and plotting helpers
  - `chlaa_plot_ppc()` for posterior predictive checks of cases

- `forecast_scenarios.R`, `plot_scenario_forecasts.R`
  - `chlaa_forecast_scenarios_from_fit()`: paired scenario forecasts + differences vs baseline
  - `chlaa_plot_scenario_forecasts()`: overlay or facet, absolute or differences

- `health_econ.R`
  - `chlaa_health_econ()`: costs + DALYs using model accumulators

- `optimise.R`
  - `chlaa_optimise_budget()`: constrained allocation optimiser (grid/continuous modes)

- `plot.R`
  - general plotting helpers and a CE plane plot

## Running the full stack

Typical workflow:
1. `pars <- chlaa_parameters()`
2. `sim <- chlaa_simulate(pars, ...)`
3. `fit <- chlaa_fit_pmcmc(data, pars, ...)`
4. `pars_cal <- chlaa_update_from_fit(fit, pars)`
5. `sc <- chlaa_make_aa_scenarios(pars_cal, trigger_time = ...)`
6. `runs <- chlaa_run_scenarios(pars_cal, sc, time = ...)`
7. `cmp <- chlaa_compare_scenarios(runs, baseline = "baseline", include_econ = TRUE)`
8. Forecasts:
   - `chlaa_forecast_from_fit(fit, ...)`
   - `chlaa_forecast_scenarios_from_fit(fit, scenarios = sc, ...)`

## Guardrails

- `make check-sync`: validates committed generated files against `inst/odin/generation_manifest.csv`.
- `make check-no-binaries`: fails if compiled artefacts are tracked under `src/` or `inst/`.
- CI runs both checks before `R CMD check`.
