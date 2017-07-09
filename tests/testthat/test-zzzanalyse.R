context("analyse")

test_that("analyse", {

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
                 select_data = list("Year+" = numeric(), YearFactor = factor(),
                                    Site = factor(), Density = numeric(),
                                    HabitatQuality = factor()),
                 new_expr = new_expr)

  expect_identical(parameters(model$derived), "sDensity")

  analysis <- analyse(model, data = data, glance = FALSE, beep = FALSE)

  expect_identical(parameters(analysis), sort(c("bIntercept", "bYear", "log_sDensity")))
  expect_identical(parameters(analysis, "random"), character(0))
  expect_identical(parameters(analysis, "all"), sort(c("bIntercept", "bYear", "log_sDensity", "sDensity")))
  expect_identical(parameters(analysis, "primary"), sort(c("bIntercept", "bYear", "log_sDensity")))
  expect_error(parameters(analysis, "some"))

  expect_is(as.mcmcr(analysis), "mcmcr")
  expect_identical(nchains(analysis), 1L)
  expect_identical(niters(analysis), 1L)

  glance <- glance(analysis)
  expect_is(glance, "tbl")
  expect_identical(colnames(glance), c("n", "K", "logLik", "AICc", "duration", "converged"))
  expect_equal(glance$logLik, -5238.213, tolerance = 0.0000001)
  expect_identical(glance$n, 300L)
  expect_identical(glance$K, 3L)
  expect_is(glance$duration, "Duration")

  coef <- coef(analysis)

  expect_is(coef, "tbl")
  expect_identical(colnames(coef), c("term", "estimate", "sd", "zscore", "lower", "upper", "pvalue"))

  expect_identical(coef$term, as.term(c("bIntercept", "bYear", "log_sDensity")))

  expect_identical(coef(analysis, "derived")$term, as.term("sDensity"))
  expect_identical(coef(analysis, "all")$term, as.term(c("bIntercept", "bYear", "log_sDensity", "sDensity")))

  tidy <- tidy(analysis)
  expect_identical(colnames(tidy), c("term", "estimate", "std.error", "statistic", "p.value"))
  expect_identical(tidy$estimate, coef$estimate)

  year <- predict(analysis, new_data = "Year")

  expect_is(year, "tbl")
  expect_identical(colnames(year), c("Site", "HabitatQuality", "Year", "Visit",
                                     "Density", "YearFactor",
                                     "estimate", "sd", "zscore", "lower", "upper", "pvalue"))
  expect_false(is.unsorted(year$estimate))
  expect_true(all(is.na(year$lower)))

  expect_equal(unlist(estimates(analysis)), coef$estimate, check.names = FALSE)
})
