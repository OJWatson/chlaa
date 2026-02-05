test_that("cholera_update_from_fit updates known parameters", {
  pars <- cholera_parameters()

  fake_draws <- matrix(
    c(
      0.06, 12.0, 4.0,
      0.05, 10.0, 5.0
    ),
    nrow = 2, byrow = TRUE
  )
  colnames(fake_draws) <- c("trans_prob", "contact_rate", "incubation_time")

  fit <- list(pars = fake_draws)
  class(fit) <- "cholera_fit"

  upd <- cholera_update_from_fit(fit, pars, draw = "mean", burnin = 0, thin = 1, validate = TRUE)

  expect_true(is.list(upd))
  expect_equal(upd$trans_prob, mean(c(0.06, 0.05)))
  expect_equal(upd$contact_rate, mean(c(12.0, 10.0)))
  expect_equal(upd$incubation_time, mean(c(4.0, 5.0)))
})
