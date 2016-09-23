model_code_example1 <- "
#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() () {
DATA_VECTOR(x);
DATA_VECTOR(y);

PARAMETER(a); // intercept
PARAMETER(b); // slope
PARAMETER(log_sigma);

Type sigma = exp(log_sigma);

int n = y.size();

Type nll = 0.0;
for(int i = 0; i < n; i++){
  nll -= dnorm(y[i], a + b * x[i], sigma, true);
}

return nll;
}"

data_set_example1 <- function() {
  set.seed(123)
  data <- data.frame(x = stats::runif(20, 1, 10))
  data$y = stats::rnorm(20, mean = 1.8 + 2.4 * data$x, sd = exp(0.3))
  data
}

parameters_example1 <- list(a = 0, b = 0, log_sigma = 0)

model_code_example2 <- "
#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() () {
DATA_VECTOR(x);
DATA_VECTOR(y);

PARAMETER(a); // intercept
PARAMETER(b); // slope
PARAMETER(log_sigma);

vector<Type> fit = x;

Type sigma = exp(log_sigma);

int n = y.size();

Type nll = 0.0;
for(int i = 0; i < n; i++){
  fit[i] = a + b * x[i];
  nll -= dnorm(y[i], fit[i], sigma, true);
}
ADREPORT(fit);
return nll;
}"

data_set_example2 <- function() {
  set.seed(123)
  data <- data.frame(x = stats::runif(20, 1, 10))
  data$y = stats::rnorm(20, mean = 1.8 + 2.4 * data$x, sd = exp(0.3))
  data
}

parameters_example2 <- list(a = 0, b = 0, log_sigma = 0)
