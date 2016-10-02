context("model")

test_that("model", {
  model <- tmb_model(model_code_example2, inits = inits_example2,
                     select = list(y = 1, x = 1), scale = "x")

  expect_true(is.tmb_model(model))
  expect_identical(model_code(model), model_code_example2)
  expect_identical(inits(model), inits_example2)

  expect_error(tmb_model(model_code_example2, inits = inits_example2, select = 1))
  expect_is(tmb_model(model_code_example2, inits = inits_example2, select = c("y", "x")), "tmb_model")
  expect_error(tmb_model(model_code_example2, inits = inits_example2, select = c("y", "x"),
                         scale = "z"), "columns in scale must also be in select")
})
