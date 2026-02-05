# Health economics utilities

#' Health economic outputs from scenario simulations
#'
#' Computes approximate costs and DALYs for each scenario using explicit model accumulators:
#' - cum_vax1, cum_vax2 (doses administered)
#' - cum_orc_treated, cum_ctc_treated (cases treated)
#' - cum_symptoms, cum_deaths (health outcomes)
#'
#' WASH intervention costs are optionally computed from the parameter windows if
#' `scenario_runs` has the attribute `scenario_parameters` (added by `cholera_run_scenarios()`).
#'
#' @param scenario_runs Output of `cholera_run_scenarios()`.
#' @param econ Optional named list overriding default economic parameters.
#'
#' @return A data.frame with scenario-level mean costs, DALYs, and components.
#' @export
cholera_health_econ <- function(scenario_runs, econ = NULL) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("dplyr is required for cholera_health_econ()", call. = FALSE)
  }

  defaults <- list(
    cost_per_vaccine_dose = 2.00,
    cost_per_orc_treatment = 10.0,
    cost_per_ctc_treatment = 80.0,

    cost_chlorination_per_person_day = 0.02,
    cost_hygiene_per_person_day = 0.03,
    cost_latrine_per_person_day = 0.01,
    cost_cati_per_person_day = 0.05,

    dw_symptomatic = 0.188,
    duration_symptomatic_days = 7.0,
    yll_per_death = 30.0
  )

  if (!is.null(econ)) {
    .check_named_list(econ, "econ")
    defaults[names(econ)] <- econ
  }
  econ <- defaults

  req <- c(
    "scenario","time","particle",
    "cum_symptoms","cum_deaths",
    "cum_orc_treated","cum_ctc_treated",
    "cum_vax1","cum_vax2"
  )
  missing <- setdiff(req, names(scenario_runs))
  if (length(missing) > 0) {
    stop("scenario_runs missing required columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  end <- scenario_runs |>
    dplyr::group_by(.data$scenario, .data$particle) |>
    dplyr::filter(.data$time == max(.data$time)) |>
    dplyr::ungroup()

  out <- end |>
    dplyr::mutate(
      doses_total = .data$cum_vax1 + .data$cum_vax2,
      cost_vax = econ$cost_per_vaccine_dose * .data$doses_total,
      cost_care = econ$cost_per_orc_treatment * .data$cum_orc_treated +
        econ$cost_per_ctc_treatment * .data$cum_ctc_treated,
      yld = econ$dw_symptomatic * (econ$duration_symptomatic_days / 365) * .data$cum_symptoms,
      yll = econ$yll_per_death * .data$cum_deaths,
      dalys = .data$yld + .data$yll
    )

  pars_by_scenario <- attr(scenario_runs, "scenario_parameters")
  if (is.null(pars_by_scenario) || !is.list(pars_by_scenario)) {
    out$cost_wash <- 0.0
  } else {
    t_min <- min(scenario_runs$time)
    t_max <- max(scenario_runs$time) + 1

    wash_cost_for <- function(p) {
      N <- p$N
      dur <- function(start, end) {
        a <- max(t_min, start)
        b <- min(t_max, end)
        max(0, b - a)
      }

      d_chlor <- dur(p$chlor_start, p$chlor_end)
      d_hyg <- dur(p$hyg_start, p$hyg_end)
      d_lat <- dur(p$lat_start, p$lat_end)
      d_cati <- dur(p$cati_start, p$cati_end)

      econ$cost_chlorination_per_person_day * d_chlor * N +
        econ$cost_hygiene_per_person_day * d_hyg * N +
        econ$cost_latrine_per_person_day * d_lat * N +
        econ$cost_cati_per_person_day * d_cati * N
    }

    wash_cost_lookup <- vapply(names(pars_by_scenario), function(nm) wash_cost_for(pars_by_scenario[[nm]]), numeric(1))
    out$cost_wash <- wash_cost_lookup[out$scenario]
  }

  out <- out |>
    dplyr::mutate(total_cost = .data$cost_vax + .data$cost_care + .data$cost_wash)

  out |>
    dplyr::group_by(.data$scenario) |>
    dplyr::summarise(
      mean_cost = mean(.data$total_cost),
      mean_cost_vax = mean(.data$cost_vax),
      mean_cost_care = mean(.data$cost_care),
      mean_cost_wash = mean(.data$cost_wash),
      mean_dalys = mean(.data$dalys),
      mean_cases_symptomatic = mean(.data$cum_symptoms),
      mean_deaths = mean(.data$cum_deaths),
      mean_doses = mean(.data$doses_total),
      mean_orc_treated = mean(.data$cum_orc_treated),
      mean_ctc_treated = mean(.data$cum_ctc_treated),
      .groups = "drop"
    )
}
