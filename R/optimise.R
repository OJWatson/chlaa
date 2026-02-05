# Budget optimisation utilities

#' Optimise intervention allocation under a budget constraint
#'
#' Simple grid-search optimiser for allocating budget across vaccination and WASH, with
#' optional constraints. This is a pragmatic starting point; swap to a more sophisticated
#' optimiser once costs and decision variables are finalised.
#'
#' The objective is to minimise expected deaths (mean across particles) over the horizon.
#'
#' @param pars Baseline parameter list.
#' @param budget Total budget.
#' @param cost Cost list (see Details).
#' @param time Simulation times.
#' @param n_particles Number of particles for each evaluation.
#' @param dt Time step.
#' @param seed Seed.
#' @param grid_size Number of grid points per decision dimension.
#'
#' @details
#' `cost` is a named list that can include:
#' - cost_per_vaccine_dose
#' - cost_chlorination_per_person_day
#' - cost_hygiene_per_person_day
#' - cost_latrine_per_person_day
#' - cost_cati_per_person_day
#'
#' Budget is spent on:
#' - vaccination doses (vax1_total_doses, vax1_doses_per_day within the campaign window)
#' - WASH (implemented by setting intervention effects; this is a placeholder mapping)
#'
#' @return A list with best allocation and a data.frame of evaluated allocations.
#' @export
cholera_optimise_budget <- function(pars,
                                   budget,
                                   cost = list(
                                     cost_per_vaccine_dose = 2.0,
                                     cost_chlorination_per_person_day = 0.02,
                                     cost_hygiene_per_person_day = 0.03,
                                     cost_latrine_per_person_day = 0.01,
                                     cost_cati_per_person_day = 0.05
                                   ),
                                   time = 0:180,
                                   n_particles = 100,
                                   dt = 0.25,
                                   seed = 1,
                                   grid_size = 10) {
  .check_named_list(pars, "pars")
  .check_named_list(cost, "cost")
  if (!is.numeric(budget) || length(budget) != 1 || budget <= 0) stop("budget must be > 0", call. = FALSE)

  # Decision variables: fraction of budget to vaccine (rest to WASH)
  frac_vax <- seq(0, 1, length.out = grid_size)

  eval <- vector("list", length(frac_vax))
  for (i in seq_along(frac_vax)) {
    fv <- frac_vax[i]
    b_vax <- budget * fv
    b_wash <- budget * (1 - fv)

    # Map budget to doses (dose 1 only for now)
    doses <- floor(b_vax / cost$cost_per_vaccine_dose)

    # Map WASH budget to a scalar intensity and then to effects (placeholder)
    # Interpret intensity as proportion of maximal effect achievable.
    # Person-days in horizon: N * duration (rough). Use duration based on max time.
    dur <- max(time) - min(time) + 1
    denom <- pars$N * dur * (
      cost$cost_chlorination_per_person_day +
        cost$cost_hygiene_per_person_day +
        cost$cost_latrine_per_person_day +
        cost$cost_cati_per_person_day
    )
    intensity <- if (denom > 0) min(1, b_wash / denom) else 0

    p <- pars
    # Vaccination campaign: start at day 0 for `dur_vax` days
    dur_vax <- min(14, dur)
    p$vax1_start <- min(time)
    p$vax1_end <- min(time) + dur_vax
    p$vax1_total_doses <- doses
    p$vax1_doses_per_day <- if (dur_vax > 0) doses / dur_vax else 0

    # WASH effects (placeholder)
    p$chlor_start <- min(time); p$chlor_end <- max(time) + 1; p$chlor_effect <- 0.3 * intensity
    p$hyg_start <- min(time); p$hyg_end <- max(time) + 1; p$hyg_effect <- 0.3 * intensity
    p$lat_start <- min(time); p$lat_end <- max(time) + 1; p$lat_effect <- 0.2 * intensity
    p$cati_start <- min(time); p$cati_end <- max(time) + 1; p$cati_effect <- 0.2 * intensity

    cholera_parameters_validate(p)

    sim <- cholera_simulate(p, time = time, n_particles = n_particles, dt = dt, seed = seed + i)
    # Expected deaths at end
    end_time <- max(sim$time)
    end <- sim[sim$time == end_time, , drop = FALSE]
    deaths <- mean(end$cum_deaths)

    eval[[i]] <- data.frame(
      frac_vax = fv,
      budget_vax = b_vax,
      budget_wash = b_wash,
      doses = doses,
      wash_intensity = intensity,
      deaths = deaths,
      stringsAsFactors = FALSE
    )
  }

  res <- do.call(rbind, eval)
  best <- res[which.min(res$deaths), , drop = FALSE]
  list(best = best, evaluations = res)
}
