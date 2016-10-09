context("analyse")

test_that("analyse", {
  model <- tmb_model(model_code_example2, inits = inits_example2, random = "bYear",
                     new_code = "fit <- a + b * x;
                     for(i in 1:length(x)) residual[i] <- y[i] - (a + b * x[i] + bYear[Year[i]]) ")

  analysis <- analyse(model, data_set_example2, beep = FALSE)

  expect_true(is.tmb_analysis(analysis))

  expect_identical(model_code(analysis), model_code_example2)
  expect_equal(data_set(analysis), data_set_example2)
  expect_equal(parameters(analysis), sort(c("a", "b", "log_sigma", "log_sYear")))
  expect_equal(parameters(analysis, "report"), c("fit"))
  expect_equal(parameters(analysis, "adreport"), c("residual"))
  expect_equal(parameters(analysis, "random"), c("bYear"))

  model <- tmb_model(model_code_example2, inits = inits_example2,
                     select_data = c("x", "y", "z"))

  expect_error(tmb_analysis(data_set_example2, model, beep = FALSE), "column names in data must include 'x', 'y' and 'z'")

  model <- tmb_model(model_code_example2, inits = inits_example2,
                     select_data = list(x = 1, y = TRUE))
  expect_error(tmb_analysis(data_set_example2, model, beep = FALSE), "column y in data must be of class 'logical'")

  expect_equal(logLik(analysis), -3213.379, tolerance = 1e-7)
  expect_equal(glance(analysis), data.frame(logLik = -3213.379), tolerance = 1e-7)

  coef <- coef(analysis)
  expect_is(coef, "tbl")
  expect_identical(colnames(coef), c("term", "estimate", "std.error", "statistic", "p.value", "lower", "upper"))
  expect_identical(coef$lower, coef$estimate - coef$std.error * qnorm(0.975))
  expect_equal(coef$upper, coef$estimate + coef$std.error * qnorm(0.975))
  expect_identical(coef, tidy(analysis))

  fixed <- coef(analysis)
  expect_identical(nrow(fixed), 4L)

  report <- coef(analysis, terms = "adreport")
  expect_identical(nrow(report), 1000L)

  expect_error(coef(analysis, terms = "report"), "terms must only include values which match the regular expressions")

  expect_identical(fixed, tidy(analysis))
  fit <- fitted(analysis)
  expect_identical(names(fit), c("x", "y", "Year", "fit"))
  expect_identical(residuals(analysis)$residual, augment(analysis)$residual)
  expect_identical(ncol(augment(analysis)), 5L)

  prediction <- predict(analysis, new_code = "for (i in 1:length(x)) prediction[i] <- a + b * x[i]")
  expect_is(prediction, "tbl")
  expect_identical(colnames(prediction), c("x", "y", "Year", "prediction"))
  expect_identical(data_set_example2, as.data.frame(prediction[c("x", "y", "Year")]))

  expect_identical(fit$fit, prediction$prediction)
#  expect_equal(residuals(analysis)$residual, report$estimate[report$term == "residual"])

  prediction2 <- predict(analysis, term = "other", new_code =
                           "for (i in 1:length(x)) {
    prediction[i] <- a + b * x[i]
  }
other <- a + b * x")
  expect_identical(colnames(prediction2), c("x", "y", "Year", "other"))
  expect_identical(prediction$prediction, prediction2$other)

  prediction3 <- predict(analysis, new_data = data_set_example2[3,], term = "other", new_code =
                           "for (i in 1:length(x)) {
    prediction[i] <- a + b * x[i]
  }
other <- a + b * x")
  expect_equal(prediction2[3,], prediction3)
})
