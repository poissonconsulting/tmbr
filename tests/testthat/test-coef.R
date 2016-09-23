context("tmb-analysis")

test_that("coef", {
  model <- tmb_model(model_code_example2, parameters = parameters_example2)

  analysis <- tmb_analysis(data_set_example2(), model)

  coef <- coef(analysis, conf.int = TRUE)
  expect_is(coef, "data.frame")
  expect_identical(colnames(coef), c("term", "estimate", "std.error", "statistic", "p.value", "lower", "upper"))
  expect_identical(coef[c("term", "estimate", "std.error", "statistic", "p.value")], coef(analysis))
  expect_identical(coef, tidy(analysis, conf.int = TRUE))

  fixed <- coef(analysis)
  expect_identical(nrow(fixed), 3L)

  report <- coef(analysis, terms = "report")
  expect_identical(nrow(report), 20L)

  all <- coef(analysis, terms = "all")
  expect_identical(nrow(all), 23L)

  expect_identical(fixed, tidy(analysis))

  expect_error(coef(analysis, terms = "random"))
  expect_error(coef(analysis, terms = "report", conf.int = TRUE))
})
