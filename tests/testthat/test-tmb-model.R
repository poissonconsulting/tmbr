context("tmb-model")

test_that("tmb_model", {
  model <- tmb_model(model_code_example1, parameters = parameters_example1)

  expect_true(is.tmb_model(model))
  expect_identical(model_code(model), model_code_example1)
  expect_identical(parameters(model), parameters_example1)

  expect_error(tmb_model(model_code, parameters = parameters, select_data = 1))
})
