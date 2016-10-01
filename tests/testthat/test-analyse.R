context("tmb-analysis")

test_that("tmb_analysis", {
  model <- tmb_model(model_code_example2, parameters = parameters_example2)

  expect_true(is.tmb_model(model))
  expect_identical(model_code(model), model_code_example2)
  expect_identical(parameters(model), parameters_example2)

  expect_error(tmb_model(model_code, parameters = parameters_example2, select_data = 1))

  analysis <- analyse(model, data_set_example2, beep = FALSE)

  expect_true(is.tmb_analysis(analysis))

  expect_identical(model_code(analysis), model_code_example2)
  expect_identical(parameters(analysis), parameters_example2)
  expect_equal(data_set(analysis), data_set_example2)

  model <- tmb_model(model_code_example2, parameters = parameters_example2,
                     select_data = c("x", "y", "z"))

  expect_error(tmb_analysis(data_set_example2, model, beep = FALSE), "column names in data_set must include 'x', 'y' and 'z'")

  model <- tmb_model(model_code_example2, parameters = parameters_example2,
                     select_data = list(x = 1, y = TRUE))
  expect_error(tmb_analysis(data_set_example2, model, beep = FALSE), "column y in data_set must be of class 'logical'")

  expect_equal(logLik(analysis), -32.31632, tolerance = 1e-7)
  expect_equal(glance(analysis), data.frame(logLik = -32.31632), tolerance = 1e-7)

  coef <- coef(analysis, conf_int = TRUE)
  expect_is(coef, "data.frame")
  expect_identical(colnames(coef), c("term", "estimate", "std.error", "statistic", "p.value", "lower", "upper"))
  expect_identical(coef[c("term", "estimate", "std.error", "statistic", "p.value")], coef(analysis, conf_level = 0))
  expect_identical(coef, tidy(analysis, conf.int = TRUE))

  fixed <- coef(analysis, conf_int = TRUE)
  expect_identical(nrow(fixed), 3L)

  report <- coef(analysis, terms = "report")
  expect_identical(nrow(report), 20L)

  all <- coef(analysis, terms = "all")
  expect_identical(nrow(all), 23L)

  expect_identical(fixed, tidy(analysis, conf.int = TRUE))

  expect_error(coef(analysis, terms = "report", conf_int = TRUE))
})
