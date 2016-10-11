context("analyse")

test_that("analyse", {
  model <- tmb_model(model_code_example2, gen_inits = gen_inits_example2, random_effects = list(bYear = "Year"),
                     new_expr = "
                    fit2 <- a + b * x
                    fit <- a + b * x + bYear[Year]
                     residual <- y - fit
                     prediction <- fit")

  analysis <- analyse(model, data_set_example2, beep = FALSE)

  expect_true(is.tmb_analysis(analysis))

  expect_identical(model_code(analysis), model_code_example2)
  expect_equal(data_set(analysis), data_set_example2)
  expect_equal(logLik(analysis), -3213.379, tolerance = 1e-7)

  coef <- coef(analysis)
  expect_is(coef, "tbl")
  expect_identical(colnames(coef), c("term", "estimate", "std.error", "statistic", "p.value", "lower", "upper"))
  expect_identical(coef$lower, coef$estimate - coef$std.error * qnorm(0.975))
  expect_equal(coef$upper, coef$estimate + coef$std.error * qnorm(0.975))
  expect_identical(nrow(coef), 4L)

  adreport <- coef(analysis, terms = "adreport")
  expect_identical(nrow(adreport), 1000L)

  random <- coef(analysis, terms = "random")
  expect_identical(nrow(random), 10L)

  expect_error(coef(analysis, terms = "report"), "terms must only include values which match the regular expressions")

  fit <- fitted(analysis)
  residuals <- residuals(analysis)
  expect_identical(names(fit), c("x", "y", "Year", "estimate"))

  prediction <- predict(analysis)
  expect_is(prediction, "tbl")
  expect_identical(colnames(prediction), c("x", "y", "Year", "estimate"))
  expect_identical(data_set_example2, as.data.frame(prediction[c("x", "y", "Year")]))
  expect_identical(nrow(prediction), 1000L)
  expect_identical(predict(analysis, new_data = data_set(analysis)[10,])$estimate, predict(analysis, term = "fit2")$estimate[10])

  expect_identical(prediction$estimate, fit$estimate)
  expect_equal(residuals$estimate, adreport$estimate[adreport$term == "residual"])
  expect_equal(data_set_example2$y, fit$estimate + residuals$estimate)

  prediction2 <- predict(analysis, new_data = data_set(analysis), term = "other", new_expr =
                           "prediction <- a + b * x + bYear[Year]
                            other <- prediction")
  expect_identical(colnames(prediction2), c("x", "y", "Year", "estimate"))
  expect_identical(prediction$estimate, prediction2$estimate)

  prediction3 <- predict(analysis, new_data = data_set_example2[3,], term = "other", new_expr =
                           "other <- a + b * x + bYear[Year]")
  expect_equal(prediction2[3,], prediction3)

  estimates <- estimates(analysis)
  expect_identical(lapply(estimates, dims), lapply(analysis$inits[names(estimates)], dims))
  estimates <- estimates(analysis, "random")
  expect_identical(estimates$bYear, random$estimate)
  estimates <- estimates(analysis, "report")
  expect_equal(estimates$fit, fit$estimate, tolerance = 1e-5)
  estimates <- estimates(analysis, "adreport")
  expect_equal(estimates$residual, residuals$estimate)

  expect_identical(lincomb_names(analysis),
                   c("log_sigma", "a", "b", "log_sYear"))

  expect_identical(names(named_estimates(estimates(analysis, "random"))), paste0("bYear[", 1:10, "]"))
  expect_equal(named_estimates(estimates(analysis, "random")), estimates(analysis, "random")$bYear,
               check.attributes = FALSE)

  profile <- predict(analysis, data_set_example2[1:2,], "prediction <- exp(2 * 3 + - 7)", conf_int = TRUE)
  expect_identical(colnames(profile), c("x", "y", "Year", "estimate", "lower", "upper"))
  expect_identical(profile$lower, rep(exp(2 * 3 + - 7), 2))
  expect_identical(profile$lower, profile$upper)
  profile <- predict(analysis, data_set_example2[1:2,], "prediction <- exp(2 * 3 + - 7  + 2 * bYear[Year])", conf_int = TRUE)
  expect_identical(profile$lower, profile$estimate)
  expect_equal(profile$lower, exp(2 * 3 + -7 + 2 * estimates(analysis, "random")$bYear[as.integer(data_set_example2$Year[1:2])]))

  expect_error(predict(analysis, data_set_example2[1:2,], "prediction <- a + b * x + bYear2[Year]", conf_int = TRUE), "unrecognised parameter name")
  expect_equal(predict(analysis, data_set_example2[3,], "prediction <- fit[Year] + bYear[Year]")$estimate,
                   predict(analysis, data_set_example2[3,], "prediction <- fit[Year] + bYear[Year]", conf_int = TRUE)$estimate)
  profile <- predict(analysis, data_set_example2[1:2,], "prediction <- a + b * x + bYear[Year] + 1 + -1", conf_int = TRUE)
  expect_equal(profile$estimate, prediction$estimate[1:2])
  expect_equal(profile$lower[2], 45.36208, tolerance = 1e-6)
  expect_equal(profile$estimate[2], 53.05689, tolerance = 1e-6)
  expect_equal(profile$upper[2], 60.69323, tolerance = 1e-6)
})
