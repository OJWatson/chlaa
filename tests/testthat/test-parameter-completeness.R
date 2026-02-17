test_that("parameter metadata matches generator parameter sets", {
  skip_if_not_installed("dust2")

  info_names <- sort(unique(cholera_parameter_info()$name))

  sim_gen <- chlaa:::cholera_generator("simulate")
  fit_gen <- chlaa:::cholera_generator("fit")
  gen_names <- sort(unique(c(
    attr(sim_gen, "parameters")$name,
    attr(fit_gen, "parameters")$name
  )))

  expect_setequal(info_names, gen_names)
})

test_that("runtime odin2 compilation gate is explicit opt-in", {
  old <- getOption("chlaa.allow_runtime_odin2")
  on.exit(options(chlaa.allow_runtime_odin2 = old), add = TRUE)

  options(chlaa.allow_runtime_odin2 = FALSE)
  Sys.setenv(CHLAA_ALLOW_RUNTIME_ODIN2 = "false")
  expect_false(chlaa:::.runtime_odin2_allowed())

  options(chlaa.allow_runtime_odin2 = TRUE)
  expect_true(chlaa:::.runtime_odin2_allowed())

  options(chlaa.allow_runtime_odin2 = FALSE)
  Sys.setenv(CHLAA_ALLOW_RUNTIME_ODIN2 = "true")
  expect_true(chlaa:::.runtime_odin2_allowed())

  Sys.unsetenv("CHLAA_ALLOW_RUNTIME_ODIN2")
})
