test_that("pmcmc runs with the bundled fit generator (tiny smoke test)", {
  skip_if_not_installed("dust2")
  skip_if_not_installed("monty")
  skip_on_cran()

  pars <- chlaa_parameters()

  # Keep cases at zero to ensure finite initial likelihood at default parameters.
  data <- data.frame(
    time = 1:3,
    cases = c(0L, 0L, 0L)
  )

  fit <- chlaa_fit_pmcmc(
    data = data,
    pars = pars,
    n_particles = 16,
    n_steps = 8,
    seed = 1,
    proposal_var = 0.01
  )

  expect_s3_class(fit, "chlaa_fit")

  draws <- chlaa_fit_draws(fit)
  expect_true(is.matrix(draws))
  expect_equal(nrow(draws), 8)
  expect_true(ncol(draws) > 0)
  expect_true(all(is.finite(draws)))

  fc <- chlaa_forecast_from_fit(
    fit = fit,
    pars = pars,
    time = data$time,
    vars = c("expected_cases", "cum_expected_cases"),
    include_cases = TRUE,
    obs_model = "mean",
    n_draws = 2,
    burnin = 0,
    seed = 2,
    dt = 1,
    deterministic = TRUE
  )

  expect_true(all(c("expected_cases", "cum_expected_cases", "cases") %in% fc$variable))
  cases <- fc[fc$variable == "cases", , drop = FALSE]
  expected_cases <- fc[fc$variable == "expected_cases", , drop = FALSE]
  expect_equal(cases$mean, expected_cases$mean)
})
