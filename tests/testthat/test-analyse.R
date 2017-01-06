context("analyse")

test_that("analyse", {

  require(newdata)

  data <- density99
  data$Year <- factor(data$Year)

  tmb_template <- "
#include <TMB.hpp>

  template<class Type>
  Type objective_function<Type>::operator() () {
  DATA_VECTOR(Density);
  DATA_FACTOR(Year);
  DATA_FACTOR(Site);

  DATA_INTEGER(nSite);
  DATA_INTEGER(nYear);

  PARAMETER(bIntercept);
  PARAMETER(bYear);
  PARAMETER_VECTOR(bSite);
  PARAMETER_MATRIX(bSiteYear);

  PARAMETER(log_sSiteYear);
  PARAMETER(log_sDensity);

  Type sSiteYear = exp(log_sSiteYear);
  Type sDensity = exp(log_sDensity);

  vector<Type> eDensity = Density;

  Type nll = 0.0;

  for(int i = 0; i < nSite; i++){
    for(int j = 0; j < nYear; j++){
      nll -= dnorm(bSiteYear(i,j), Type(0.0), sSiteYear, true);
    }
  }

  for(int i = 0; i < Density.size(); i++){
    eDensity(i) = exp(bIntercept + bYear * Year(i) + bYear + bSite(Site(i)) + bSiteYear(Site(i),Year(i)));
    nll -= dnorm(Density(i), log(eDensity(i)), sDensity ,true);
  }
  return nll;
}"

  new_expr <- "
  for(i in 1:length(Density)) {
    prediction[i] <- bIntercept + bYear * Year[i] + bSite[Site[i]] + bSiteYear[Site[i], Year[i]]
  } "

  gen_inits <- function(data) list(bIntercept = 1, bYear = 0, bSite = rep(0, data$nSite), log_sSiteYear = 0, log_sDensity = 0)

  model <- model(tmb_template, gen_inits = gen_inits, monitor = "^(b|s)",
                 random_effects = list(bSiteYear = c("Site", "Year")),
                 new_expr = new_expr)

  analysis <- analyse(model, data = data, beep = FALSE)

  expect_identical(parameters(mb_code(tmb_template)), c("bIntercept", "bSite", "bSiteYear", "bYear", "log_sDensity", "log_sSiteYear"))
  expect_identical(parameters(mb_code(tmb_template)), sort(c(parameters(analysis), parameters(analysis, FALSE))))

  coef <- coef(analysis)

  expect_is(coef, "tbl")
  expect_identical(colnames(coef), c("term", "estimate", "std.error", "statistic",
                                     "p.value", "lower", "upper"))

  expect_identical(coef$term, c("bIntercept", "bSite[1]", "bSite[2]", "bSite[3]",
                                "bSite[4]", "bSite[5]", "bSite[6]", "bYear",
                                "log_sDensity", "log_sSiteYear"))

  predict <- predict(analysis, new_data = new_data(data, "Site"))

#  expect_is(predict, "tbl")
#  expect_identical(colnames(predict), c("Density", "Site", "Year", "Visit", "estimate"))
#  expect_identical(nrow(predict), 6L)

  #  analysis <- reanalyse(analysis, beep = FALSE)

  #  expect_identical(niters(analysis), 500L)
  #  expect_identical(nchains(analysis), 4L)
  #  expect_identical(nsamples(analysis), 2000L)

  #  expect_equal(convergence(analysis), 1.00, tolerance = 1)

  #  expect_is(as.mcmcr(analysis), "mcmcr")
})
