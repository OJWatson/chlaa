# Fitting utilities (pMCMC)

#' Fit the cholera model to incidence data using pMCMC (monty + dust2)
#'
#' Expects a data.frame with columns:
#' - time: numeric times (days)
#' - cases: integer case counts at those times
#'
#' Likelihood is defined inside `inst/odin/cholera_model_fit.R`:
#' cases ~ NegativeBinomial(mu = reporting_rate * inc_symptoms, size = obs_size)
#'
#' @param data Data frame with columns time and cases.
#' @param pars Starting parameter list.
#' @param n_particles Number of particles for the dust2 filter likelihood.
#' @param n_steps Number of MCMC steps.
#' @param seed Random seed.
#' @param prior Optional monty prior model. If NULL, uses `chlaa_default_prior()`.
#' @param packer Optional monty packer. If NULL, uses `chlaa_default_packer()`.
#' @param proposal_var Proposal variance (scalar) for diagonal random walk.
#'
#' @return A `chlaa_fit` object (also keeps monty class) with attributes:
#'   packer, prior, start_pars, data.
#' @export
chlaa_fit_pmcmc <- function(data,
                             pars,
                             n_particles = 200,
                             n_steps = 2000,
                             seed = 1,
                             prior = NULL,
                             packer = NULL,
                             proposal_var = 0.02) {
  .check_named_list(pars, "pars")
  if (!requireNamespace("monty", quietly = TRUE)) stop("monty is required for fitting", call. = FALSE)

  data <- chlaa_prepare_data(data, time_col = "time", cases_col = "cases")

  gen <- chlaa_generator(which = "fit")
  # dust2 expects the filter start time to be strictly earlier than the first
  # observation time.
  time_start <- min(data$time) - 1

  filter <- dust2::dust_filter_create(gen, time_start = time_start, data = data, n_particles = n_particles)

  if (is.null(packer)) {
    packer <- chlaa_default_packer(pars)
  }
  if (is.null(prior)) {
    prior <- chlaa_default_prior()
  }

  likelihood <- dust2::dust_likelihood_monty(filter, packer)
  posterior <- prior + likelihood

  packer_names <- packer[["names"]]()
  sampler <- monty::monty_sampler_random_walk(diag(length(packer_names)) * proposal_var)

  set.seed(seed)
  initial_vec <- packer[["pack"]](pars)

  res <- monty::monty_sample(posterior, sampler, n_steps, initial = initial_vec)

  attr(res, "packer") <- packer
  attr(res, "prior") <- prior
  attr(res, "start_pars") <- pars
  attr(res, "data") <- data

  class(res) <- unique(c("chlaa_fit", class(res)))
  res
}

chlaa_default_prior <- function() {
  if (!requireNamespace("monty", quietly = TRUE)) stop("monty is required for fitting", call. = FALSE)
  monty::monty_dsl({
    trans_prob ~ Uniform(0.001, 0.2)
    contact_rate ~ Uniform(0.1, 30)
    incubation_time ~ Uniform(1, 10)
    duration_sym ~ Uniform(3, 30)
    reporting_rate ~ Uniform(0.01, 1.0)
    obs_size ~ Uniform(1, 200)
    seek_severe ~ Uniform(0.1, 1.0)
    fatality_untreated ~ Uniform(0.05, 0.9)
    fatality_treated ~ Uniform(0.0001, 0.05)
  })
}

chlaa_default_packer <- function(pars) {
  if (!requireNamespace("monty", quietly = TRUE)) stop("monty is required for fitting", call. = FALSE)
  .check_named_list(pars, "pars")

  # Keep this order consistent with `chlaa_default_prior()` (monty uses
  # positional parameter matching).
  names_fit <- c(
    "trans_prob", "contact_rate",
    "incubation_time", "duration_sym",
    "reporting_rate", "obs_size",
    "seek_severe",
    "fatality_untreated", "fatality_treated"
  )

  fixed <- pars[setdiff(names(pars), names_fit)]
  monty::monty_packer(names_fit, fixed = fixed)
}
