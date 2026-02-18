# Synthetic example data helpers

#' Generate A Synthetic Multi-Wave Cholera Outbreak Time Series
#'
#' Generates daily synthetic observed case counts with a pronounced first wave,
#' temporary suppression, and a smaller rebound wave. The generator uses the
#' package's simulation model plus Negative Binomial observation noise.
#'
#' This is intended for examples/vignettes where we want realistic outbreak
#' shape without shipping real line-list data.
#'
#' @param time Numeric vector of simulation times (days).
#' @param start_date Optional start date; if not NULL, a `date` column is added.
#' @param seed Integer random seed.
#' @param n_particles Number of particles used to generate the latent incidence.
#' @param dt Model time step.
#' @param obs_size Observation over-dispersion (Negative Binomial size).
#'
#' @return A data.frame with columns `time`, `cases`, `mu_cases`,
#'   `inc_symptoms_truth`, and (optionally) `date`.
#'   The generating parameter set is attached as `attr(x, "truth_parameters")`.
#' @export
chlaa_generate_example_outbreak_data <- function(time = 1:330,
                                                 start_date = as.Date("2021-08-01"),
                                                 seed = 42,
                                                 n_particles = 1,
                                                 dt = 1,
                                                 obs_size = 20) {
  if (!is.numeric(time) || length(time) < 2) {
    stop("time must be a numeric vector with length >= 2", call. = FALSE)
  }
  time <- sort(as.numeric(time))

  pars <- chlaa_parameters()

  # Tuned to yield a shape qualitatively similar to observed multi-wave
  # outbreaks: large early peak, decline, and later rebound.
  pars$contact_rate <- 8
  pars$trans_prob <- 0.022
  pars$reporting_rate <- 0.03
  pars$obs_size <- obs_size

  # Temporary response period that suppresses transmission before a rebound.
  pars$chlor_start <- 85
  pars$chlor_end <- 185
  pars$chlor_effect <- 0.55
  pars$hyg_start <- 85
  pars$hyg_end <- 185
  pars$hyg_effect <- 0.55
  pars$lat_start <- 85
  pars$lat_end <- 185
  pars$lat_effect <- 0.35
  pars$cati_start <- 85
  pars$cati_end <- 185
  pars$cati_effect <- 0.35

  sim <- chlaa_simulate(
    pars = pars,
    time = time,
    n_particles = n_particles,
    dt = dt,
    seed = seed
  )

  sim_mean <- stats::aggregate(
    sim$inc_symptoms,
    by = list(time = sim$time),
    FUN = mean
  )

  mu_cases <- pmax(pars$reporting_rate * sim_mean$x, 0.01)

  set.seed(seed)
  cases <- stats::rnbinom(length(mu_cases), mu = mu_cases, size = obs_size)

  out <- data.frame(
    time = sim_mean$time,
    cases = as.integer(cases),
    mu_cases = as.numeric(mu_cases),
    inc_symptoms_truth = as.numeric(sim_mean$x),
    stringsAsFactors = FALSE
  )

  if (!is.null(start_date)) {
    if (!inherits(start_date, "Date") || length(start_date) != 1) {
      stop("start_date must be NULL or a single Date", call. = FALSE)
    }
    out$date <- start_date + (out$time - min(out$time))
    out <- out[, c("date", "time", "cases", "mu_cases", "inc_symptoms_truth")]
  }

  attr(out, "truth_parameters") <- pars
  out
}
