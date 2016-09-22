context("tmb-coef")

test_that("tmb_coef", {
  model_code <- "
#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() () {

DATA_VECTOR(x);
DATA_VECTOR(y);

PARAMETER(a);
PARAMETER(b);
PARAMETER(log_sigma);

vector<Type> fit = x;

Type sigma = exp(log_sigma);

int n = y.size();

Type nll = 0.0;

for(int i = 0; i < n; i++){
  fit[i] = a + b * x[i];
  nll -= dnorm(y[i], a + b * x[i], sigma, true);
}
ADREPORT(fit);
return nll;
}"

  parameters <- list(a = 0, b = 0, log_sigma = 0)
  model <- tmb_model(model_code, parameters = parameters)

  set.seed(123)
  data <- data.frame(x = runif(20, 1, 10))
  data$y = rnorm(20, mean = 1.8 + 2.4 * data$x, sd = exp(0.3))

  analysis <- tmb_analysis(data, model)

  coef <- coef(analysis, conf.int = TRUE)
  expect_is(coef, "data.frame")
  expect_identical(colnames(coef), c("term", "estimate", "std.error", "statistic", "p.value", "lower", "upper"))
  expect_identical(coef[c("term", "estimate", "std.error", "statistic", "p.value")], coef(analysis))
  expect_identical(coef, tidy(analysis, conf.int = TRUE))

  fixed <- tidy(analysis)
  expect_identical(nrow(fixed), 3L)

  report <- tidy(analysis, terms = "report")
  expect_identical(nrow(report), 20L)

  all <- tidy(analysis, terms = "all")
  expect_identical(nrow(all), 23L)

  expect_error(tidy(analysis, terms = "random"))
  expect_error(tidy(analysis, terms = "report", conf.int = TRUE))
})
