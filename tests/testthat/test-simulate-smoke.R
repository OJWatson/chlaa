test_that("simulation runs (optional smoke test)", {
  if (tolower(Sys.getenv("CHLAA_RUN_SIM_TESTS")) != "true") {
    skip("Set CHLAA_RUN_SIM_TESTS=true to run simulation compilation smoke tests")
  }
  skip_if_not_installed("odin2")
  skip_if_not_installed("dust2")

  pars <- cholera_parameters()
  sim <- cholera_simulate(pars, time = 0:2, n_particles = 1, dt = 1, seed = 1)

  expect_true(is.data.frame(sim))
  expect_true(all(c("time", "particle", "inc_symptoms", "cum_deaths") %in% names(sim)))
})
