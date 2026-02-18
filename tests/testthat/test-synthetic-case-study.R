test_that("chlaa_case_study_setup returns coherent shared objects", {
  cs <- chlaa_case_study_setup(time = 0:120, seed = 1, n_particles = 4)

  expect_true(is.list(cs))
  expect_true(all(c("data", "pars", "scenarios", "dates", "time") %in% names(cs)))
  expect_true(is.data.frame(cs$data))
  expect_true(all(c("date", "time", "cases", "mu_cases", "inc_symptoms_truth", "inc_infections_truth") %in% names(cs$data)))

  expect_equal(length(cs$scenarios), 4)
  expect_true(all(vapply(cs$scenarios, function(x) inherits(x, "chlaa_scenario"), logical(1))))

  d2 <- chlaa_generate_example_outbreak_data(time = 0:120, seed = 1, n_particles = 4)
  expect_equal(cs$data$cases, d2$cases)
  expect_equal(cs$data$time, d2$time)
})
