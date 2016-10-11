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
  nll -= dnorm(y(i), a + b * x(i), sigma, true);
}

return nll;
}"

gen_inits_example1 <- function(data) list(a = 0, b = 0, log_sigma = 0)

model_code_example2 <- "
#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() () {
DATA_VECTOR(x);
DATA_VECTOR(y);
DATA_FACTOR(Year);

PARAMETER(log_sigma);
PARAMETER_VECTOR(bYear);
PARAMETER(a); // intercept
PARAMETER(b); // slope
PARAMETER(log_sYear);

vector<Type> fit = x;
vector<Type> residual = x;

Type sigma = exp(log_sigma);
Type sYear = exp(log_sYear);

int nYear = bYear.size();
int n = y.size();

Type zero = 0.0;

Type nll = 0.0;

for(int i = 0; i < nYear; i++){
  nll -= dnorm(bYear(i), zero, sYear, true);
}

for(int i = 0; i < n; i++){
  fit(i) = a + b * x(i) + bYear(Year(i));
  residual(i) = y(i) - fit(i);
  nll -= dnorm(y(i), fit(i), sigma, true);
}
REPORT(fit);
ADREPORT(residual);
return nll;
}"

gen_inits_example2 <- function(data) list(a = 0, b = 0, log_sigma = 0, bYear = rep(0, 10), log_sYear = 0)

. <- NULL


