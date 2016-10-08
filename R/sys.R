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

inits_example1 <- list(a = 0, b = 0, log_sigma = 0)

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
vector<Type> residual = x;

Type sigma = exp(log_sigma);

int n = y.size();

Type nll = 0.0;
for(int i = 0; i < n; i++){
  fit[i] = a + b * x[i];
  residual[i] = y[i] - fit[i];
  nll -= dnorm(y[i], fit[i], sigma, true);
}
REPORT(fit);
ADREPORT(residual);
return nll;
}"

inits_example2 <- list(a = 0, b = 0, log_sigma = 0)

. <- NULL


