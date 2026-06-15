# chlaa interactive model explorer

This Shiny app is a package-bundled workspace for exploring how the `chlaa`
cholera outbreak model is fitted to data and how fitted outbreaks can be used
for counterfactual and anticipatory-action scenario analysis.

## What the model represents

`chlaa` simulates a cholera outbreak in a closed population. People move
through susceptible, exposed, infectious, recovered, vaccinated, treated, and
death states. The process model includes symptomatic and asymptomatic
infection, environmental contamination, care seeking, treatment capacity,
vaccination, and WASH-style transmission reductions.

The fitted likelihood compares observed reported cases with model-generated
incidence. For weekly data, the model uses `inc_symptoms_weekly`; for daily
data it uses `inc_symptoms`. Reported cases are treated as a noisy observation
of symptomatic incidence through a reporting fraction and a Negative Binomial
dispersion parameter.

## How fitting works

The package fitting workflow starts with an outbreak case-count time series:

```r
dat <- chlaa_prepare_data(raw_cases, time_col = "time", cases_col = "cases")
fit <- chlaa_fit_pmcmc(
  data = dat,
  pars = chlaa_parameters(N = 515741, E0 = 20),
  obs_interval = 7,
  n_particles = 50,
  n_steps = 2000,
  n_chains = 3
)
```

The fitted object stores posterior draws for key epidemiological and
observation parameters. Those draws can then be projected back through the
transmission model to produce posterior predictive checks:

```r
fc <- chlaa_forecast_from_fit(
  fit = fit,
  pars = attr(fit, "start_pars"),
  time = dat$time,
  vars = "inc_symptoms_weekly",
  include_cases = TRUE,
  obs_interval = 7
)

chlaa_plot_forecast(fc, var = "cases", data = dat, data_y = "cases")
```

If the ribbons cover the main timing and scale of the observed case counts, the
fit is a plausible baseline for scenario analysis. It is still important to
check trace plots, posterior summaries, and whether the fitted parameters make
epidemiological sense.

## Understanding interventions that already happened

The Kirotshe example bundled with the package includes a pMCMC fit and recorded
intervention timings. In the app's Kirotshe tab, the fitted response is the
baseline scenario: it represents the outbreak as fitted with the recorded
response assumptions. A no-intervention counterfactual switches off WASH,
case-management capacity, and vaccination levers.

Comparing the fitted response with no intervention gives a model-based estimate
of how much the recorded response may already have reduced cases or deaths.
This is not causal proof by itself. It is a structured counterfactual: the same
fitted outbreak is re-simulated after removing selected intervention effects.

## Exploring anticipatory action

Anticipatory-action scenarios use the same fitted outbreak as a starting point,
but alter the timing, intensity, or composition of the response package. Common
questions include:

- What if WASH activities had started when weekly cases first crossed a trigger?
- What if oral rehydration points and cholera treatment centres were available
  earlier?
- What if a one-dose vaccination campaign was started before the peak?
- How sensitive are results to the response duration, effect size, or campaign
  speed?

The app turns those questions into parameter modifications. For example, an
early combined package may set WASH effect windows, treatment capacity windows,
and vaccine campaign windows:

```r
scenario <- chlaa_scenario("early_combined_response", list(
  chlor_start = trigger_time,
  chlor_end = trigger_time + 120,
  chlor_effect = 0.20,
  hyg_start = trigger_time,
  hyg_end = trigger_time + 120,
  hyg_effect = 0.20,
  orc_start = trigger_time,
  orc_end = trigger_time + 120,
  ctc_start = trigger_time,
  ctc_end = trigger_time + 120,
  vax1_start = trigger_time + 14,
  vax1_end = trigger_time + 42,
  vax1_total_doses = 100000,
  vax1_doses_per_day = 100000 / 28
))
```

`chlaa_forecast_scenarios_from_fit()` then simulates the baseline,
no-intervention reference, and custom scenarios using matched posterior draws.
Matched draws reduce Monte Carlo noise in differences between scenarios.

## Reading the plots and metrics

The case-incidence plot overlays scenario trajectories with the default fitted
model. The observed points remain visible so that the baseline fit can be judged
against the data. Saved scenarios can be shown or hidden without deleting their
outputs.

The cumulative-case metric reports the mean cumulative symptomatic cases at the
end of the simulated horizon. The percentage reduction is computed against the
no-intervention reference:

```r
100 * (cases_no_intervention - cases_scenario) / cases_no_intervention
```

Positive values indicate fewer cumulative cases than the no-intervention
counterfactual. Negative values indicate more cases than the no-intervention
counterfactual, which can happen because of stochastic variation, implausible
parameter choices, or interventions that shift timing without reducing the
total burden.

## Cost-benefit and economic ideas

The package includes health-economic helpers that convert scenario trajectories
into costs, DALYs, incremental cost-effectiveness ratios, and net monetary
benefit. The economic defaults are transparent placeholders and should be
replaced with local costs before decision use.

A typical workflow is:

```r
runs <- chlaa_run_scenarios(pars_fit, scenarios, scenario_time)
cmp <- chlaa_compare_scenarios(
  runs,
  baseline = "fitted_response",
  include_econ = TRUE,
  wtp = 1500
)
chlaa_plot_ce_plane(cmp)
```

In practice, decision makers can use the app for first-pass exploration, then
export saved scenario parameters and rerun preferred scenarios in a scripted
analysis with more posterior draws, more particles, local costs, and explicit
sensitivity analyses.

## Using your own data

The custom-data tab accepts a CSV with a time column and a case-count column.
Weekly data should usually have times `7, 14, 21, ...`; daily data can use
`1, 2, 3, ...` or another regular daily index. After fitting, the same scenario
controls become available.

For quick app use, start with a small number of MCMC steps and particles. For
publication or operational decisions, run the package fitting workflow outside
the app with longer chains, multiple starting points, trace diagnostics, and
documented prior assumptions.
