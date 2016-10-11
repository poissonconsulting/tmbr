
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis-CI Build Status](https://travis-ci.org/poissonconsulting/tmbr.svg?branch=master)](https://travis-ci.org/poissonconsulting/tmbr) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/poissonconsulting/tmbr?branch=master&svg=true)](https://ci.appveyor.com/project/poissonconsulting/tmbr) [![codecov](https://codecov.io/gh/poissonconsulting/tmbr/branch/master/graph/badge.svg)](https://codecov.io/gh/poissonconsulting/tmbr)

tmbr
====

Introduction
------------

`tmbr` (pronounced timber) is an R package to facilitate analyses using Template Model Builder (TMB).

Demonstration
-------------

``` r
library(tmbr)
#> Loading required package: mbr

model_code <- "
#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() () {
DATA_VECTOR(wt);
DATA_VECTOR(hp);
DATA_VECTOR(disp);
DATA_VECTOR(mpg);

PARAMETER(bIntercept); 
PARAMETER(bWt); 
PARAMETER(bHp); 
PARAMETER(bDisp); 
PARAMETER(log_sigma);

Type sigma = exp(log_sigma);

int n = wt.size();

Type nll = 0.0;
for(int i = 0; i < n; i++){
  nll -= dnorm(mpg[i], bIntercept + bWt * wt[i] + bHp * hp[i] + bDisp * disp[i], sigma, true);
}

return nll;
}"

gen_inits <- function(data) list(bIntercept = 0, bWt = 0, bHp = 0, bDisp = 0, log_sigma = 0)

model <- tmb_model(model_code, gen_inits = gen_inits, scale = c("wt", "hp", "disp"))

analysis <- analyse(model, data = mtcars)

coef(analysis)
#> # A tibble: 5 × 7
#>         term   estimate std.error   statistic      p.value      lower
#>        <chr>      <dbl>     <dbl>       <dbl>        <dbl>      <dbl>
#> 1 bIntercept 20.0906136 0.4363725 46.04005491 0.000000e+00 19.2353392
#> 2        bWt -3.7191704 0.9758459 -3.81122705 1.382787e-04 -5.6317933
#> 3        bHp -2.1362272 0.7334318 -2.91264586 3.583808e-03 -3.5737272
#> 4      bDisp -0.1159350 1.1998888 -0.09662145 9.230270e-01 -2.4676739
#> 5  log_sigma  0.9036089 0.1250001  7.22886649 4.870419e-13  0.6586132
#> # ... with 1 more variables: upper <dbl>
logLik(analysis)
#> [1] -74.32149

analysis2 <- lm(mpg ~ wt + hp + disp, 
        data = rescale::rescale(mtcars, scale = c("wt", "hp", "disp")))

summary(analysis2)
#> 
#> Call:
#> lm(formula = mpg ~ wt + hp + disp, data = rescale::rescale(mtcars, 
#>     scale = c("wt", "hp", "disp")))
#> 
#> Residuals:
#>    Min     1Q Median     3Q    Max 
#> -3.891 -1.640 -0.172  1.061  5.861 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)  20.0906     0.4665  43.067  < 2e-16 ***
#> wt           -3.7190     1.0432  -3.565  0.00133 ** 
#> hp           -2.1362     0.7841  -2.724  0.01097 *  
#> disp         -0.1161     1.2827  -0.091  0.92851    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 2.639 on 28 degrees of freedom
#> Multiple R-squared:  0.8268, Adjusted R-squared:  0.8083 
#> F-statistic: 44.57 on 3 and 28 DF,  p-value: 8.65e-11
logLik(analysis)
#> [1] -74.32149
```

Installation
------------

To install from GitHub

    # install.packages("devtools")
    devtools::install_github("poissonconsulting/tmbr")

Contribution
------------

Please report any [issues](https://github.com/poissonconsulting/tmbr/issues).

[Pull requests](https://github.com/poissonconsulting/tmbr/pulls) are always welcome.

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.

Inspiration
-----------

-   [jaggernaut](https://github.com/poissonconsulting/jaggernaut)

Documentation
-------------

-   [TMB](https://github.com/kaskr/adcomp)
