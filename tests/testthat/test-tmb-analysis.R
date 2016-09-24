context("tmb-analysis")

test_that("tmb_analysis", {
  model <- tmb_model(model_code_example1, parameters = parameters_example1)

  analysis <- tmb_analysis(data_set_example1, model)

  expect_true(is.tmb_analysis(analysis))

  expect_identical(model_code(analysis), model_code_example1)
  expect_identical(parameters(analysis), parameters_example1)
  expect_equal(data_set(analysis), data_set_example1)
})
