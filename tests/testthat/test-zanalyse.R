context("analyse")

test_that("analyse", {
  model <- tmb_model(model_code_example2, gen_inits = gen_inits_example2, random = list(bYear = "Year"),
                     predict_expr = " fit2 <- a + b * x
                    for(i in 1:length(x)) {
                     fit[i] <- a + b * x[i] + bYear[Year[i]]
                     residual[i] <- y[i] - fit[i]
                     }
                     prediction <- fit")

  analysis <- analyse(model, data_set_example2, beep = FALSE)

  expect_true(is.tmb_analysis(analysis))

  expect_identical(model_code(analysis), model_code_example2)
  expect_equal(data_set(analysis), data_set_example2)
  expect_equal(logLik(analysis), -3213.379, tolerance = 1e-7)
  expect_equal(glance(analysis), data.frame(logLik = -3213.379), tolerance = 1e-7)

  coef <- coef(analysis)
  expect_is(coef, "tbl")
  expect_identical(colnames(coef), c("term", "estimate", "std.error", "statistic", "p.value", "lower", "upper"))
  expect_identical(coef$lower, coef$estimate - coef$std.error * qnorm(0.975))
  expect_equal(coef$upper, coef$estimate + coef$std.error * qnorm(0.975))
  expect_identical(coef, tidy(analysis))
  expect_identical(nrow(coef), 4L)

  adreport <- coef(analysis, terms = "adreport")
  expect_identical(nrow(adreport), 1000L)

  random <- coef(analysis, terms = "random")
  expect_identical(nrow(random), 10L)

  expect_error(coef(analysis, terms = "report"), "terms must only include values which match the regular expressions")

  expect_identical(coef, tidy(analysis))
  fit <- fitted(analysis)
  residuals <- residuals(analysis)
  expect_identical(names(fit), c("x", "y", "Year", "fit"))
  expect_identical(residuals$residual, augment(analysis)$residual)
  expect_identical(ncol(augment(analysis)), 5L)

  prediction <- predict(analysis)
  expect_is(prediction, "tbl")
  expect_identical(colnames(prediction), c("x", "y", "Year", "prediction"))
  expect_identical(data_set_example2, as.data.frame(prediction[c("x", "y", "Year")]))

  expect_identical(prediction$prediction, fit$fit)
  expect_equal(residuals$residual, adreport$estimate[adreport$term == "residual"])
  expect_equal(data_set_example2$y, fit$fit + residuals$residual)

  prediction2 <- predict(analysis, term = "other", predict_expr =
                           "for (i in 1:length(x)) {
    prediction[i] <- a + b * x[i] + bYear[Year[i]]
  }
other <- prediction")
  expect_identical(colnames(prediction2), c("x", "y", "Year", "other"))
  expect_identical(prediction$prediction, prediction2$other)

  prediction3 <- predict(analysis, new_data = data_set_example2[3,], term = "other", predict_expr =
                           "for (i in 1:length(x)) {
    other[i] <- a + b * x[i] + bYear[Year[i]]
  }")
  expect_equal(prediction2[3,], prediction3)

  estimates <- estimates(analysis)
  expect_identical(lapply(estimates, dims), lapply(analysis$inits[names(estimates)], dims))
  estimates <- estimates(analysis, "random")
  expect_identical(estimates$bYear, random$estimate)
  estimates <- estimates(analysis, "report")
  expect_equal(estimates$fit, fit$fit, tolerance = 1e-5)
  estimates <- estimates(analysis, "adreport")
  expect_equal(estimates$residual, residuals$residual)
})
