test_that("cholera_prepare_data validates and normalises input", {
  dat <- data.frame(t = c(1, 3, 2), y = c(1, 2, 3))
  out <- cholera_prepare_data(dat, time_col = "t", cases_col = "y")
  expect_equal(names(out), c("time", "cases"))
  expect_equal(out$time, c(1, 2, 3))
  expect_equal(out$cases, c(1L, 3L, 2L))

  out2 <- cholera_prepare_data(
    data.frame(time = c(1, 3), cases = c(2, 4)),
    expected_step = 1,
    fill_missing = TRUE
  )
  expect_equal(out2$time, c(1, 2, 3))
  expect_equal(out2$cases, c(2L, 0L, 4L))

  expect_error(cholera_prepare_data(data.frame(time = c(1, 1), cases = c(0, 1))))
  expect_error(cholera_prepare_data(data.frame(time = 1:3, cases = c(1, -1, 2))))
})

test_that("fit report and trace helpers return expected structures", {
  skip_if_not_installed("dust2")
  skip_if_not_installed("monty")

  pars <- cholera_parameters()
  data <- data.frame(time = 1:3, cases = c(0L, 0L, 0L))
  fit <- cholera_fit_pmcmc(
    data = data,
    pars = pars,
    n_particles = 16,
    n_steps = 10,
    seed = 1,
    proposal_var = 0.01
  )

  tr <- cholera_fit_trace(fit)
  expect_true(is.data.frame(tr))
  expect_true(all(c("iteration", "parameter", "value") %in% names(tr)))

  rpt <- cholera_fit_report(fit, burnin = 0, thin = 1)
  expect_true(is.list(rpt))
  expect_true(is.numeric(rpt$acceptance_rate))
  expect_true(rpt$acceptance_rate >= 0 && rpt$acceptance_rate <= 1)
  expect_true(is.data.frame(rpt$posterior_summary))
})
