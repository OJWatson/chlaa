# Repository schema (chlaa)

This is a standalone R package implementing a stochastic cholera outbreak model in **odin2** (dust2 backend),
with utilities for fitting, counterfactual simulation, forecasting, and health economic evaluation.

End users should not need odin2 installed: the repository commits the generated C++ and bindings so simulation and fitting work from a clean checkout. odin2 is only required for developers who want to edit `inst/odin/*.R` and regenerate the bundled outputs (see `docs/packaging.md`).

## Directory layout

- `DESCRIPTION`, `NAMESPACE`, `LICENSE`: standard R package metadata
- `R/`: package code (functions)
- `inst/odin/`: odin2 model files
  - `cholera_model.R`: simulation model
  - `cholera_model_fit.R`: adds an observation model for filtering / likelihood
- `man/`: prebuilt documentation (keeps R CMD check quiet without roxygen)
- `tests/`: unit tests (simulation smoke tests are opt-in via env var)

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
  - `cholera_parameter_info()`: default values + metadata
  - `cholera_parameters()`: build parameter list with overrides
  - `cholera_parameters_validate()`: basic sanity checks

- `model.R`
  - `cholera_simulate()`: run the model via dust2
  - `cholera_generator()`: internal generator getter (packaged generator or runtime odin2 compile)

- `fit.R`
  - `cholera_fit_pmcmc()`: particle MCMC fitting (monty + dust2)

- `fit_object.R`
  - `cholera_fit` S3 helpers (`print`, `summary`, `cholera_posterior_summary()`)

- `posterior.R`
  - draw extraction + parameter updating (`cholera_update_from_fit()`)
  - posterior predictive simulations (`cholera_simulate_posterior()`)

- `scenario_builder.R`
  - `cholera_trigger_time_from_sim()`
  - `cholera_make_aa_scenarios()`: AA scenario templates

- `scenario.R`
  - `cholera_scenario()`, `cholera_run_scenarios()`
  - `cholera_compare_scenarios()` (optionally with economics)

- `counterfactual_grid.R`
  - `cholera_counterfactual_grid()`: tidy grid of scenario knobs
  - `cholera_run_counterfactual_grid()`

- `forecast.R`
  - `cholera_forecast_from_fit()` and plotting helpers
  - `cholera_plot_ppc()` for posterior predictive checks of cases

- `forecast_scenarios.R`, `plot_scenario_forecasts.R`
  - `cholera_forecast_scenarios_from_fit()`: paired scenario forecasts + differences vs baseline
  - `cholera_plot_scenario_forecasts()`: overlay or facet, absolute or differences

- `health_econ.R`
  - `cholera_health_econ()`: costs + DALYs using model accumulators

- `optimise.R`
  - `cholera_optimise_budget()`: simple grid-search budget allocation optimiser

- `plot.R`
  - general plotting helpers and a CE plane plot

## Running the full stack

Typical workflow:
1. `pars <- cholera_parameters()`
2. `sim <- cholera_simulate(pars, ...)`
3. `fit <- cholera_fit_pmcmc(data, pars, ...)`
4. `pars_cal <- cholera_update_from_fit(fit, pars)`
5. `sc <- cholera_make_aa_scenarios(pars_cal, trigger_time = ...)`
6. `runs <- cholera_run_scenarios(pars_cal, sc, time = ...)`
7. `cmp <- cholera_compare_scenarios(runs, baseline = "baseline", include_econ = TRUE)`
8. Forecasts:
   - `cholera_forecast_from_fit(fit, ...)`
   - `cholera_forecast_scenarios_from_fit(fit, scenarios = sc, ...)`
