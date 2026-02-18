# Synthetic example data helpers

.chlaa_case_study_day <- function(date, start_date) {
  as.numeric(as.Date(date) - as.Date(start_date))
}

.chlaa_case_study_base_parameters <- function(obs_size) {
  # Paper-aligned defaults (population and broad epidemiological scale):
  # https://pmc.ncbi.nlm.nih.gov/articles/PMC12477517/
  chlaa_parameters(
    N = 540000,
    Sev0 = 0.2,
    contact_rate = 2.8,
    trans_prob = 0.005,
    reporting_rate = 0.12,
    obs_size = obs_size,
    chlor_start = 0, chlor_end = 0, chlor_effect = 0,
    hyg_start = 0, hyg_end = 0, hyg_effect = 0,
    lat_start = 0, lat_end = 0, lat_effect = 0,
    cati_start = 0, cati_end = 0, cati_effect = 0,
    orc_start = 0, orc_end = 0,
    ctc_start = 0, ctc_end = 0,
    vax1_start = 0, vax1_end = 0, vax1_doses_per_day = 0, vax1_total_doses = 0,
    vax2_start = 0, vax2_end = 0, vax2_doses_per_day = 0, vax2_total_doses = 0
  )
}

.chlaa_case_study_scenarios <- function(pars,
                                        start_date,
                                        trigger_date,
                                        declaration_date,
                                        late_vax_start_date,
                                        vax_total_doses,
                                        campaign_days) {
  trigger_day <- .chlaa_case_study_day(trigger_date, start_date)
  declaration_day <- .chlaa_case_study_day(declaration_date, start_date)
  late_vax_start_day <- .chlaa_case_study_day(late_vax_start_date, start_date)

  response_duration <- 240
  response_end <- declaration_day + response_duration

  response_effects <- list(
    chlor_end = response_end, chlor_effect = 0.45,
    hyg_end = response_end, hyg_effect = 0.45,
    lat_end = response_end, lat_effect = 0.25,
    cati_end = response_end, cati_effect = 0.25,
    orc_end = response_end,
    ctc_end = response_end
  )

  response_from <- function(start_day) {
    c(
      list(
        chlor_start = start_day,
        hyg_start = start_day,
        lat_start = start_day,
        cati_start = start_day,
        orc_start = start_day,
        ctc_start = start_day
      ),
      response_effects
    )
  }

  late_campaign <- list(
    vax1_start = late_vax_start_day,
    vax1_end = late_vax_start_day + campaign_days,
    vax1_total_doses = vax_total_doses,
    vax1_doses_per_day = vax_total_doses / campaign_days,
    vax2_start = 0, vax2_end = 0,
    vax2_total_doses = 0, vax2_doses_per_day = 0
  )

  early_one_dose <- list(
    vax1_start = trigger_day + 30,
    vax1_end = trigger_day + 30 + campaign_days,
    vax1_total_doses = vax_total_doses,
    vax1_doses_per_day = vax_total_doses / campaign_days,
    vax2_start = 0, vax2_end = 0,
    vax2_total_doses = 0, vax2_doses_per_day = 0
  )

  # Keep the second-dose campaign conservative to avoid infeasible
  # administration trajectories in stochastic particles when many first-dose
  # recipients leave eligibility compartments before dose 2.
  vax2_total_stable <- min(vax_total_doses / 2, 40000)

  early_two_dose <- list(
    vax1_start = trigger_day + 30,
    vax1_end = trigger_day + 30 + campaign_days,
    vax1_total_doses = vax_total_doses / 2,
    vax1_doses_per_day = (vax_total_doses / 2) / campaign_days,
    vax2_start = trigger_day + 30 + campaign_days + 14,
    vax2_end = trigger_day + 30 + campaign_days + 14 + campaign_days,
    vax2_total_doses = vax2_total_stable,
    vax2_doses_per_day = vax2_total_stable / campaign_days
  )

  list(
    chlaa_scenario("scenario_1_baseline", c(response_from(declaration_day), late_campaign)),
    chlaa_scenario("scenario_2_anticipatory_action", c(response_from(trigger_day), late_campaign)),
    chlaa_scenario("scenario_3_anticipatory_action_plus_one_vaccine_dose", c(response_from(trigger_day), early_one_dose)),
    chlaa_scenario("scenario_4_anticipatory_action_plus_two_vaccine_doses", c(response_from(trigger_day), early_two_dose))
  )
}

#' Build A Shared Paper-Aligned Cholera Case Study Setup
#'
#' Constructs a reusable synthetic case study aligned to the outbreak response
#' framing in:
#' https://pmc.ncbi.nlm.nih.gov/articles/PMC12477517/
#'
#' The returned object includes:
#' - one synthetic observed case time series (generated from scenario 1 baseline),
#' - calibrated baseline parameters,
#' - scenario definitions used throughout the vignettes.
#'
#' @param time Numeric vector of simulation times (days).
#' @param start_date Start date corresponding to `time = 0`.
#' @param trigger_date Anticipatory action trigger date.
#' @param declaration_date Outbreak declaration date.
#' @param late_vax_start_date Planned campaign start date for scenarios 1 and 2.
#' @param seed Integer random seed.
#' @param n_particles Number of particles used to generate latent incidence.
#' @param dt Model time step.
#' @param obs_size Observation over-dispersion (Negative Binomial size).
#' @param vax_total_doses Total vaccine doses used in scenario definitions.
#' @param campaign_days Vaccination campaign duration (days).
#'
#' @return A named list with `data`, `pars`, `scenarios`, `dates`, and `time`.
#'   `data` includes columns `date`, `time`, `cases`, `mu_cases`,
#'   `inc_symptoms_truth`, and `inc_infections_truth`.
#' @export
chlaa_case_study_setup <- function(time = 0:915,
                                   start_date = as.Date("2022-07-01"),
                                   trigger_date = as.Date("2022-10-25"),
                                   declaration_date = as.Date("2022-12-14"),
                                   late_vax_start_date = as.Date("2023-01-20"),
                                   seed = 42,
                                   n_particles = 20,
                                   dt = 1,
                                   obs_size = 18,
                                   vax_total_doses = 280000,
                                   campaign_days = 150) {
  if (!is.numeric(time) || length(time) < 2) {
    stop("time must be a numeric vector with length >= 2", call. = FALSE)
  }
  time <- sort(as.numeric(time))

  date_args <- list(
    start_date = start_date,
    trigger_date = trigger_date,
    declaration_date = declaration_date,
    late_vax_start_date = late_vax_start_date
  )
  for (nm in names(date_args)) {
    if (!inherits(date_args[[nm]], "Date") || length(date_args[[nm]]) != 1) {
      stop(nm, " must be a single Date", call. = FALSE)
    }
  }
  if (!(trigger_date < declaration_date && declaration_date <= late_vax_start_date)) {
    stop("Expected trigger_date < declaration_date <= late_vax_start_date", call. = FALSE)
  }

  pars <- .chlaa_case_study_base_parameters(obs_size = obs_size)
  scenarios <- .chlaa_case_study_scenarios(
    pars = pars,
    start_date = start_date,
    trigger_date = trigger_date,
    declaration_date = declaration_date,
    late_vax_start_date = late_vax_start_date,
    vax_total_doses = vax_total_doses,
    campaign_days = campaign_days
  )

  truth <- chlaa_run_scenarios(
    pars = pars,
    scenarios = scenarios[1],
    time = time,
    n_particles = n_particles,
    dt = dt,
    seed = seed
  )

  truth_sym <- stats::aggregate(truth$inc_symptoms, by = list(time = truth$time), FUN = mean)
  truth_inf <- stats::aggregate(truth$inc_infections, by = list(time = truth$time), FUN = mean)

  mu_cases <- pmax(pars$reporting_rate * truth_sym$x, 0.01)

  set.seed(seed)
  cases <- stats::rnbinom(length(mu_cases), mu = mu_cases, size = obs_size)

  out <- data.frame(
    date = start_date + truth_sym$time,
    time = truth_sym$time,
    cases = as.integer(cases),
    mu_cases = as.numeric(mu_cases),
    inc_symptoms_truth = as.numeric(truth_sym$x),
    inc_infections_truth = as.numeric(truth_inf$x),
    stringsAsFactors = FALSE
  )

  attr(out, "truth_parameters") <- pars
  attr(out, "paper_reference") <- "https://pmc.ncbi.nlm.nih.gov/articles/PMC12477517/"

  list(
    data = out,
    pars = pars,
    scenarios = scenarios,
    dates = list(
      start_date = start_date,
      trigger_date = trigger_date,
      declaration_date = declaration_date,
      late_vax_start_date = late_vax_start_date
    ),
    time = time
  )
}

#' Generate A Synthetic Cholera Outbreak Time Series
#'
#' Convenience wrapper around `chlaa_case_study_setup()` that returns only the
#' synthetic observed case data frame.
#'
#' @inheritParams chlaa_case_study_setup
#'
#' @return A data.frame with columns `date`, `time`, `cases`, `mu_cases`,
#'   `inc_symptoms_truth`, and `inc_infections_truth`.
#'   The generating parameter set is attached as `attr(x, "truth_parameters")`.
#' @export
chlaa_generate_example_outbreak_data <- function(time = 0:915,
                                                 start_date = as.Date("2022-07-01"),
                                                 trigger_date = as.Date("2022-10-25"),
                                                 declaration_date = as.Date("2022-12-14"),
                                                 late_vax_start_date = as.Date("2023-01-20"),
                                                 seed = 42,
                                                 n_particles = 20,
                                                 dt = 1,
                                                 obs_size = 18,
                                                 vax_total_doses = 280000,
                                                 campaign_days = 150) {
  chlaa_case_study_setup(
    time = time,
    start_date = start_date,
    trigger_date = trigger_date,
    declaration_date = declaration_date,
    late_vax_start_date = late_vax_start_date,
    seed = seed,
    n_particles = n_particles,
    dt = dt,
    obs_size = obs_size,
    vax_total_doses = vax_total_doses,
    campaign_days = campaign_days
  )$data
}
