context("analyse")

test_that("analyse", {

  require(newdata)

  data <- density99
  data$YearFactor <- factor(data$Year)

  tmb_template <- "
#include <TMB.hpp>

  template<class Type>
  Type objective_function<Type>::operator() () {

  DATA_VECTOR(Density);
  DATA_VECTOR(Year);

  PARAMETER(bIntercept);
  PARAMETER(bYear);

  PARAMETER(log_sDensity);

  Type sDensity = exp(log_sDensity);

  vector<Type> eDensity = Density;

  ADREPORT(sDensity)

  Type nll = 0.0;

  for(int i = 0; i < Density.size(); i++){
    eDensity(i) = exp(bIntercept  + bYear * Year(i));
    nll -= dnorm(Density(i), log(eDensity(i)), sDensity ,true);
  }
  return nll;
}"

  new_expr <- "
  for(i in 1:length(Density)) {
    prediction[i] <- exp(bIntercept + bYear * Year[i])
  } "

  gen_inits <- function(data) list(bIntercept = 0, bYear = 1, log_sDensity = 0)

  model <- model(tmb_template, gen_inits = gen_inits,
                 center = "Year",
                 new_expr = new_expr)

  analysis <- analyse(model, data = data, beep = FALSE)

 expect_identical(parameters(analysis), sort(c("bIntercept", "bYear", "log_sDensity")))
  expect_identical(parameters(analysis, "random"), character(0))

  expect_is(as.mcmcr(analysis), "mcmcr")
  expect_identical(nchains(analysis), 1L)
  expect_identical(niters(analysis), 1L)

  glance <- glance(analysis)
  expect_is(glance, "tbl")
  expect_identical(colnames(glance), c("n", "K", "logLik", "AICc", "minutes", "converged"))
  expect_equal(glance$logLik, -5238.213, tolerance = 0.0000001)
  expect_identical(glance$n, 300L)
  expect_identical(glance$K, 3L)

  coef <- coef(analysis)

  expect_is(coef, "tbl")
  expect_identical(colnames(coef), c("term", "estimate", "sd", "zscore", "lower", "upper", "significance"))

  expect_identical(coef$term, c("bIntercept", "bYear", "log_sDensity"))

  coef2 <- coef(analysis, "derived")
  expect_identical(nrow(coef2), 1L)

  tidy <- tidy(analysis)
  expect_identical(colnames(tidy), c("term", "estimate", "std.error", "statistic", "p.value"))
  expect_identical(tidy$estimate, coef$estimate)

  year <- predict(analysis, new_data = new_data(data, "Year"))

  expect_is(year, "tbl")
  expect_identical(colnames(year), c("Site", "HabitatQuality", "Year", "Visit",
                                     "Density", "YearFactor",
                                     "estimate", "lower", "upper"))
  expect_false(is.unsorted(year$estimate))
  expect_true(all(is.na(year$lower)))

  expect_equal(unlist(estimates(analysis)), coef$estimate, check.names = FALSE)

  #  analysis <- reanalyse(analysis, beep = FALSE)

  #  expect_identical(niters(analysis), 500L)
  #  expect_identical(nchains(analysis), 4L)
  #  expect_identical(nsamples(analysis), 2000L)

  #  expect_equal(convergence(analysis), 1.00, tolerance = 1)

  #  expect_is(as.mcmcr(analysis), "mcmcr")
  #    expect_identical(estimates(analysis), estimates(analysis, mcmc = TRUE))

})
