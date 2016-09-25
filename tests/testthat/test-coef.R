context("tmb-analysis")

test_that("coef", {
  model <- tmb_model(model_code_example2, parameters = parameters_example2)

  analysis <- analyse(model, data_set_example2, beep = FALSE)

  coef <- coef(analysis, conf.int = TRUE)
  expect_is(coef, "data.frame")
  expect_identical(colnames(coef), c("term", "estimate", "std.error", "statistic", "p.value", "lower", "upper"))
  expect_identical(coef[c("term", "estimate", "std.error", "statistic", "p.value")], coef(analysis, conf_level = 0))
  expect_identical(coef, tidy(analysis, conf.int = TRUE))

  fixed <- coef(analysis)
  expect_identical(nrow(fixed), 3L)

  report <- coef(analysis, terms = "report", conf_level = 0)
  expect_identical(nrow(report), 20L)

  all <- coef(analysis, terms = "all", conf_level = 0)
  expect_identical(nrow(all), 23L)

  expect_identical(fixed, tidy(analysis, conf.int = TRUE))

  expect_error(coef(analysis, terms = "report", conf.int = TRUE))
})
