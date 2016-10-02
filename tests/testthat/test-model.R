context("model")

test_that("model", {
  model <- tmb_model(model_code_example2, inits = rev(inits_example2),
                     select = list(y = 1, x = 1), scale = "x")

  expect_true(is.tmb_model(model))
  expect_identical(model_code(model), model_code_example2)
  expect_identical(inits(model), inits_example2)
  expect_identical(dims_inits(model), list(a = 1L, b = 1L, log_sigma = 1L))

  expect_error(tmb_model(model_code_example2, inits = inits_example2, select = 1))
  expect_is(tmb_model(model_code_example2, inits = inits_example2, select = c("y", "x")), "tmb_model")
  expect_error(tmb_model(model_code_example2, inits = inits_example2, select = c("y", "x"),
                         scale = "z"), "columns in scale must also be in select")
  expect_error(tmb_model("", inits = list(a = 1, b = 1:2), random = "c"), "random effects must also be in inits")
  expect_error(tmb_model("", inits = list(a = 1, b = 1:2), random = list(c = "Year")), "random effects must also be in inits")
  expect_error(tmb_model("", inits = list(a = 1, b = 1:2), random = list(b = c("Year", "Site"))), "random effects must have the same number of dimensions as corresponding inits")
  expect_error(tmb_model("", inits = list(a = 1, b = 1:2), select = "x", random = list(b = c("Year"))), "random effects factors must also be in select")
  expect_is(tmb_model("", inits = list(a = 1, b = 1:2), select = "Year", random = list(b = c("Year"))), "tmb_model")
})
