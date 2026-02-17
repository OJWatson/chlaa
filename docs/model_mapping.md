# Model Mapping: Conceptual Structure to odin Variables

This document maps the cholera model structure used in this package to the implemented odin variables in `inst/odin/cholera_model.R` and `inst/odin/cholera_model_fit.R`.

## Stocks and transitions

| Conceptual element | odin state(s) | Notes |
|---|---|---|
| Susceptible | `S` | Receives waning returns from `Ra`, `Rs`, `V1`, `V2`; loses to infection and dose-1 vaccination. |
| Exposed (latent) | `E` | Gains from `new_E`; progresses via `new_I`. |
| Asymptomatic infectious | `A` | Gains `new_A`; recovers via `rec_A` to `Ra`. |
| Mild symptomatic (pre-triage) | `M` | Gains `new_M`; progresses via `prog_M`. |
| Severe symptomatic (pre-triage) | `Sev` | Gains `new_Sev`; progresses via `prog_Sev`. |
| Mild untreated | `Mu` | Gains `to_Mu`; exits via `leave_Mu` to `Rs`. |
| Mild treated | `Mt` | Gains `treat_orc`; exits via `leave_Mt` to `Rs`. |
| Severe untreated | `Sevu` | Gains `to_Sevu`; exits via `leave_Sevu` split into `death_Sevu` and `rec_Sevu`. |
| Severe treated | `Sevt` | Gains `treat_ctc`; exits via `leave_Sevt` split into `death_Sevt` and `rec_Sevt`. |
| Recovered post-asymptomatic | `Ra` | Waning via `wane_Ra` back to `S`. |
| Recovered post-symptomatic | `Rs` | Waning via `wane_Rs` back to `S`. |
| Vaccinated, dose 1 | `V1` | Gains `vax1_admin`; loses via infection, waning, and `vax2_admin`. |
| Vaccinated, dose 2 | `V2` | Gains `vax2_admin`; loses via infection and waning. |
| Death counters | `Du`, `Dt` | Cumulative untreated/treated deaths, updated from severe exits. |
| Environmental contamination | `C` | Continuous state updated by shedding and clearance (`dC`). |

## Incidence and cumulative outputs

| Output type | odin variable(s) | Notes |
|---|---|---|
| Daily incidence (reset each day) | `inc_infections`, `inc_symptoms`, `inc_deaths`, `inc_vax1`, `inc_vax2` | Defined with `zero_every = 1`. |
| Cumulative counters | `cum_infections`, `cum_symptoms`, `cum_deaths`, `cum_vax1`, `cum_vax2`, `cum_orc_treated`, `cum_ctc_treated` | Monotone totals used for scenario and economics summaries. |

## Interventions and controls

| Intervention | odin controls | Effect pathway |
|---|---|---|
| Chlorination, hygiene, CATI | `chlor_*`, `hyg_*`, `cati_*` | Modify transmission through `trans_mult`. |
| Latrines | `lat_*` | Modify shedding through `shed_mult`. |
| ORC/CTC | `orc_*`, `ctc_*`, capacities | Constrain treatment flows (`treat_orc`, `treat_ctc`). |
| Vaccination | `vax1_*`, `vax2_*`, `ve_1`, `ve_2` | Susceptibility reduction and compartment movement (`S -> V1 -> V2`). |

## Observation model (fit generator only)

`inst/odin/cholera_model_fit.R` adds the likelihood layer used by `cholera_fit_pmcmc()`:

- observed cases are linked to model incidence via `reporting_rate`
- a negative binomial observation model uses `obs_size`

This separation keeps simulation and fitting workflows aligned while allowing fit-specific likelihood machinery.
