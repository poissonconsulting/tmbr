context("model")

test_that("model", {
  code <- mb_code(model_code_example2)
  model <- model(code, gen_inits = gen_inits_example2,
                     select_data = list(y = 1, x = 1), scale = "x")

  expect_true(is.tmb_model(model))
  expect_identical(template(model), model_code_example2)

  expect_error(model(code, gen_inits = gen_inits_example2, select_data = 1))
  expect_is(model(code, gen_inits = gen_inits_example2, select_data = list(y = 1, x = 1)), "tmb_model")
  expect_error(model(code, gen_inits = gen_inits_example2,
                         select_data = list(y = 1, x = 1),
                         scale = "z"), "values in scale must also be in names of select_data")
  expect_error(model(code, gen_inits = list(a = 1, b = 1:2), random_effects = "c"), "gen_inits must be a function")
  expect_error(model(code, gen_inits = function () list(a = 1, b = 1:2), random_effects = "c"), "gen_inits must take a single argument")
  expect_error(model(code, gen_inits = function(data) list(a = 1, b = 1:2), random_effects = "c"), "random_effects must be a named list")
  expect_is(model(code, gen_inits = function(data) list(a = 1, b = 1:2), random_effects = list(c = "bYear")), "tmb_model")
  expect_error(model(code, gen_inits = function(data) list(a = 1, b = 1:2), select_data = list(x = 1), random_effects = list(b = c("Year"))), "elements in random_effects must also be in names of select_data")
  expect_is(model(code, gen_inits = function(data) list(a = 1, b = 1:2), select_data = list(Year = 1), random_effects = list(b = c("Year"))), "tmb_model")
  expect_error(model(code, gen_inits = function(data) list(a = 1, b = 1:2), select_data = list(Year = 1), scale = "Year", random_effects = list(b = c("Year"))), "elements in random_effects must not be in values of scale")
})

test_that("model data", {
  code <- mb_code(model_code_example2)
  model <- model(code, gen_inits = gen_inits_example2,
                     scale = "z")
  expect_error(analyse_data(data_set_example2, model, beep = FALSE), "column names in data must include 'z'")
})

test_that("model modify data", {
  code <- mb_code(model_code_example2)
  model <- model(code, gen_inits = gen_inits_example2, modify_data = function(x) stop("Houston..."))
  expect_error(analyse_data(data_set_example2, model, beep = FALSE), "Houston...")
})

test_that("make_all_models", {
  code <- mb_code(model_code_example2)
  model <- model(code, gen_inits = gen_inits_example2)
  models <- make_all_models(model, drop = list("a"))
  expect_is(models, "list")
  expect_length(models, 2L)
  expect_identical(names(models), c("-", "-a"))
  expect_identical(models[[1]], model)
  expect_identical(names(models), c("-", "-a"))
  expect_identical(names(make_all_models(model, drop = list(c("a", "b")))), c("-", "-b", "-a-b"))
  expect_identical(names(make_all_models(model, drop = list("b", "a"))), c("-", "-a", "-b", "-a-b"))
})

test_that("tmb_analysis error", {
  code <- mb_code(model_code_example2)
  model <- model(code, gen_inits = gen_inits_example2,
                     select_data = list(x = 1, y = 1, z = 1))

  expect_error(analyse_data(data_set_example2, model, beep = FALSE), "data must have column 'z'")

  model <- model(code, gen_inits = gen_inits_example2,
                     select_data = list(x = 1, y = TRUE))

  expect_error(analyse_data(data_set_example2, model, beep = FALSE), "column y in data must be of class 'logical'")

  model <- model(code, gen_inits =
                       function(data) list(a = 0, b = 0, log_sigma = 0, bYear = rep(0, 10), log_sYear = 0, bYear = c(0,0,0)), random_effects = list(bYear = "Year"),
                     select_data = list(x = 1, y = 1, Year = factor(1)))

  expect_error(analyse_data(data_set_example2, model, beep = FALSE), "dimensions of user-provided random inits must match those of random effects")

  expect_error(analyse_data(data_set_example2, model, not_an_arg = FALSE), "dots are not unused")
})

