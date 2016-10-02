context("model")

test_that("model", {
  model <- tmb_model(model_code_example2, parameters = parameters_example2,
                     select_data = list(y = 1, x = 1), scale = "x")

  expect_true(is.tmb_model(model))
  expect_identical(model_code(model), model_code_example2)
  expect_identical(parameters(model), parameters_example2)

  expect_error(tmb_model(model_code_example2, parameters = parameters_example2, select_data = 1))
  expect_is(tmb_model(model_code_example2, parameters = parameters_example2, select_data = c("y", "x")), "tmb_model")
  expect_error(tmb_model(model_code_example2, parameters = parameters_example2, select_data = c("y", "x"),
                         scale = "z"), "columns in scale must also be in select_data")
})
