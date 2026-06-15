repo_test_file <- function(...) {
  path <- file.path(getwd(), ...)
  if (!file.exists(path)) path <- file.path("..", "..", ...)
  path
}

load_shiny_app_env <- function() {
  skip_if_not_installed("shiny")
  app_path <- repo_test_file("inst", "shiny", "chlaa", "app.R")
  env <- new.env(parent = globalenv())
  source(app_path, local = env)
  env
}

test_that("custom intervention upload maps supported rows to parameter overrides", {
  env <- load_shiny_app_env()
  interventions <- utils::read.csv(
    repo_test_file("inst", "shiny", "chlaa", "examples", "oj_interventions.csv"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  overrides <- env$custom_intervention_overrides(interventions)

  expect_equal(attr(overrides, "n_interventions"), nrow(interventions))
  expect_equal(overrides$chlor_start, 28)
  expect_equal(overrides$chlor_end, 84)
  expect_equal(overrides$chlor_effect, 0.20)
  expect_equal(overrides$hyg_effect, 0.15)
  expect_equal(overrides$lat_effect, 0.10)
  expect_equal(overrides$cati_effect, 0.12)
  expect_equal(overrides$orc_capacity, 80)
  expect_equal(overrides$ctc_capacity, 25)
  expect_equal(overrides$vax1_total_doses, 12000)
  expect_equal(overrides$vax1_doses_per_day, 428.571)

  pars <- do.call(chlaa_parameters, c(list(N = 100000, E0 = 10), overrides))
  expect_equal(pars$chlor_effect, 0.20)
  expect_equal(pars$vax1_end, 77)
})

test_that("custom intervention upload reports user-friendly validation errors", {
  env <- load_shiny_app_env()

  expect_error(
    env$custom_intervention_overrides(data.frame(intervention = "mystery", start = 1, end = 2)),
    "unknown intervention name"
  )
  expect_error(
    env$custom_intervention_overrides(data.frame(intervention = "chlorination", start = NA, end = 2, effect = 0.2)),
    "missing required 'start'"
  )
  expect_error(
    env$custom_intervention_overrides(data.frame(intervention = "chlorination", start = 3, end = 2, effect = 0.2)),
    "end before start"
  )
  expect_error(
    env$custom_intervention_overrides(data.frame(intervention = "chlorination", start = "soon", end = 2, effect = 0.2)),
    "must be numeric or blank"
  )
})
