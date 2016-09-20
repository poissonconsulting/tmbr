context("tmb-analysis")

test_that("tmb_analysis", {
  model_code <- "
// linear regression
#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() () {
// data:
DATA_VECTOR(x);
DATA_VECTOR(y);

// parameters:
PARAMETER(a); // intercept
PARAMETER(b); // slope
PARAMETER(log_sigma); // log(residual SD)
// we fit sigma on a log scale to keep it > 0

// procedures: (transformed parameters)
Type sigma = exp(log_sigma);

int n = y.size(); // get number of data points to loop over

Type nll = 0.0; // initialize negative log likelihood

for(int i = 0; i < n; i++){ // C++ starts loops at 0!
  // get negative log likelihood (last argument is log = TRUE)
  nll -= dnorm(y[i], a + b * x[i], sigma, true);
}

return nll;
}"

  parameters <- list(a = 0, b = 0, log_sigma = 0)
  model <- tmb_model(model_code, parameters = parameters)

  set.seed(123)
  data <- data.frame(x = runif(20, 1, 10))
  data$y = rnorm(20, mean = 1.8 + 2.4 * data$x, sd = exp(0.3))

  analysis <- tmb_analysis(data, model)

  expect_true(is.tmb_analysis(analysis))

  expect_identical(model_code(analysis), model_code)
  expect_identical(parameters(analysis), parameters)

  tidy <- tidy(analysis, conf.int = TRUE)
  expect_is(tidy, "data.frame")
  expect_identical(colnames(tidy), c("term", "estimate", "std.error", "statistic", "p.value", "lower", "upper"))
  expect_identical(tidy[c("term", "estimate", "std.error", "statistic", "p.value")], tidy(analysis))
})
