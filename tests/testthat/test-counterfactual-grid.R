test_that("counterfactual grid returns scenarios and modify lists", {
  grid <- cholera_counterfactual_grid(
    trigger_time = 30,
    horizon = 366,
    aa_start_offset = c(0, -14),
    vax_regimen = c("none", "1dose"),
    vax_total_doses = c(0, 280000),
    vax_delay = c(0)
  )

  expect_true(is.data.frame(grid))
  expect_true(all(c("scenario", "modify") %in% names(grid)))
  expect_true(length(grid$modify) == nrow(grid))

  sc <- cholera_scenarios_from_grid(grid)
  expect_true(is.list(sc))
  expect_true(all(vapply(sc, function(x) inherits(x, "cholera_scenario"), logical(1))))
})
