test_that("cholera_parameters provides required names", {
  pars <- cholera_parameters(validate = FALSE)
  expect_true(is.list(pars))
  expect_true(!is.null(names(pars)))
  req <- cholera_parameter_info()$name
  expect_true(all(req %in% names(pars)))
})

test_that("cholera_parameters_validate catches missing parameters", {
  pars <- cholera_parameters(validate = FALSE)
  pars$N <- NULL
  expect_error(cholera_parameters_validate(pars))
})
