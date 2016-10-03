context("analyse")

test_that("analyse", {
  model <- tmb_model(model_code_example2, inits = inits_example2)

  analysis <- analyse(model, data_set_example2, beep = FALSE)

  expect_true(is.tmb_analysis(analysis))

  expect_identical(model_code(analysis), model_code_example2)
  expect_identical(inits(analysis), inits_example2)
  expect_equal(data_set(analysis), data_set_example2)

  model <- tmb_model(model_code_example2, inits = inits_example2,
                     select = c("x", "y", "z"))

  expect_error(tmb_analysis(data_set_example2, model, beep = FALSE), "column names in data_set must include 'x', 'y' and 'z'")

  model <- tmb_model(model_code_example2, inits = inits_example2,
                     select = list(x = 1, y = TRUE))
  expect_error(tmb_analysis(data_set_example2, model, beep = FALSE), "column y in data_set must be of class 'logical'")

  expect_equal(logLik(analysis), -32.31632, tolerance = 1e-7)
  expect_equal(glance(analysis), data.frame(logLik = -32.31632), tolerance = 1e-7)

  coef <- coef(analysis)
  expect_is(coef, "data.frame")
  expect_identical(colnames(coef), c("term", "estimate", "std.error", "statistic", "p.value", "lower", "upper"))
  expect_identical(coef$lower, coef$estimate - coef$std.error * qnorm(0.975))
  expect_equal(coef$upper, coef$estimate + coef$std.error * qnorm(0.975))
  expect_identical(coef, tidy(analysis))

  fixed <- coef(analysis)
  expect_identical(nrow(fixed), 3L)

  report <- coef(analysis, terms = "report")
  expect_identical(nrow(report), 40L)

  all <- coef(analysis, terms = "all")
  expect_identical(nrow(all), 43L)

  expect_identical(fixed, tidy(analysis))
  fit <- fitted(analysis)
  expect_identical(names(fit), c("x", "y", "fit.estimate", "fit.std.error",
                                 "fit.statistic", "fit.p.value", "fit.lower", "fit.upper"))
  expect_error(fitted(analysis, "blah"), "term 'blah' is not in reported terms")
  expect_identical(residuals(analysis), augment(analysis, "residual"))
  expect_identical(ncol(augment(analysis)), 14L)
})
