# load required packages

library(datasets)
library(stats)
library(TMB)

# cleanup workspace
rm(list = ls())

# input data
data <- ToothGrowth

mod <- lm(len ~ supp - 1, data = data)
summary(mod)
AIC(mod)

template <- "
#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() () {

DATA_VECTOR(len);
DATA_FACTOR(supp);

PARAMETER_VECTOR(bsupp);

PARAMETER(log_slen);

Type slen = exp(log_slen);
ADREPORT(slen);

vector<Type> elen = len;

int n = len.size();

Type nll = 0.0;

for(int i = 0; i < n; i++){
  elen(i) = bsupp(supp(i));
  nll -= dnorm(len(i), elen(i), slen, true);
}
return nll;
}"

tempfile <- tempfile()

write(template, file = paste0(tempfile, ".cpp"))

TMB::compile(paste0(tempfile, ".cpp"))

dyn.load(TMB::dynlib(tempfile))

ad_fun <- MakeADFun(data = as.list(data), parameters = list(bsupp = c(0,0), log_slen = 0), DLL = basename(tempfile))

opt <- do.call("optim", ad_fun)

sd <- sdreport(ad_fun)
summary(sd)

mod <- lm(len ~ supp, data = data)
logLik(mod)
AIC(mod)

template <- "
#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() () {

DATA_VECTOR(len);
DATA_FACTOR(supp);

PARAMETER(bintercept);
PARAMETER_VECTOR(bsupp);

PARAMETER(log_slen);

Type slen = exp(log_slen);
ADREPORT(slen);

vector<Type> elen = len;

int n = len.size();

Type nll = 0.0;

for(int i = 0; i < n; i++){
  elen(i) = bintercept + bsupp(supp(i));
  nll -= dnorm(len(i), elen(i), slen, true);
}
return nll;
}"

tempfile <- tempfile()

write(template, file = paste0(tempfile, ".cpp"))

TMB::compile(paste0(tempfile, ".cpp"))

dyn.load(TMB::dynlib(tempfile))

ad_fun <- MakeADFun(data = as.list(data), parameters = list(bintercept = 0, bsupp = c(0,0), log_slen = 0), DLL = basename(tempfile), map = list(bsupp = factor(c(NA, 2))))

opt <- do.call("optim", ad_fun)

sd <- sdreport(ad_fun)
summary(sd)

# perform last analysis using tmbr
code <- mb_code(template)
model <- model(code, gen_inits = function (data) list(bintercept = 0, bsupp = c(NA,0), log_slen = 0))
analysis <- analyse(model, data = data)
coef(analysis)
logLik(analysis)
nterms(analysis)

IC(analysis, n = Inf)
stopifnot(all.equal(IC(analysis, n = Inf), AIC(mod)))
