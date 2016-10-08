context("model")

test_that("model", {
  model <- tmb_model(model_code_example2, inits = rev(inits_example2),
                     select_data = list(y = 1, x = 1), scale = "x")

  expect_true(is.tmb_model(model))
  expect_identical(model_code(model), model_code_example2)
  expect_identical(inits(model), inits_example2)
  expect_identical(dims_inits(model), list(a = 1L, b = 1L, log_sigma = 1L))

  expect_error(tmb_model(model_code_example2, inits = inits_example2, select_data = 1))
  expect_is(tmb_model(model_code_example2, inits = inits_example2, select_data = c("y", "x")), "tmb_model")
  expect_error(tmb_model(model_code_example2, inits = inits_example2, select_data = c("y", "x"),
                         scale = "z"), "columns in scale must also be in select_data")
  expect_error(tmb_model("", inits = list(a = 1, b = 1:2), random_effects = "c"), "random effects must also be in inits")
  expect_error(tmb_model("", inits = list(a = 1, b = 1:2), random_effects = list(c = "Year")), "random effects must also be in inits")
  expect_error(tmb_model("", inits = list(a = 1, b = 1:2), random_effects = list(b = c("Year", "Site"))), "random effects must have the same number of dimensions as corresponding inits")
  expect_error(tmb_model("", inits = list(a = 1, b = 1:2), select_data = "x", random_effects = list(b = c("Year"))), "random effects factors must also be in select_data")
  expect_is(tmb_model("", inits = list(a = 1, b = 1:2), select_data = "Year", random_effects = list(b = c("Year"))), "tmb_model")
  expect_error(tmb_model("", inits = list(a = 1, b = 1:2), select_data = "Year", scale = "Year", random_effects = list(b = c("Year"))), "random effects factors must not be scaled")
  expect_error(tmb_model("", inits = list(a = 1, report = 1:2)), "inits names cannot be 'fixed', 'random', 'report' or 'adreport'")
})

test_that("model data", {
  model <- tmb_model(model_code_example2, inits = inits_example2,
                     scale = "z")
  expect_error(tmb_analysis(data_set_example2, model, beep = FALSE, debug = TRUE), "column names in data must include 'z'")
})

test_that("model modify data", {
  model <- tmb_model(model_code_example2, inits = inits_example2, modify_data = function(x) stop("Houston..."))
  expect_error(tmb_analysis(data_set_example2, model, beep = FALSE), "Houston...")
})


