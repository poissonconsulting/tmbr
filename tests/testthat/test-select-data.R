context("select-data")

test_that("select_data", {
  model <- tmb_model(model_code_example1, parameters = parameters_example1,
                     select_data = c("x", "y"))

  expect_is(tmb_analysis(data_set_example1, model, beep = FALSE), "tmb_analysis")

  model <- tmb_model(model_code_example1, parameters = parameters_example1,
                     select_data = c("x", "y", "z"))

  expect_error(tmb_analysis(data_set_example1, model, beep = FALSE), "column names in data must include 'x', 'y' and 'z'")

  model <- tmb_model(model_code_example1, parameters = parameters_example1,
                     select_data = list(x = 1, y = 1))
  expect_is(tmb_analysis(data_set_example1, model, beep = FALSE), "tmb_analysis")
})
