
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
#> Loading required package: broom

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
PARAMETER(bDisp2); 
PARAMETER(log_sigma);

Type sigma = exp(log_sigma);

int n = wt.size();

Type nll = 0.0;
for(int i = 0; i < n; i++){
  nll -= dnorm(mpg[i], bIntercept + bWt * wt[i] + bHp * hp[i] + bDisp * disp[i] + bDisp2 * pow(disp[i], 2), sigma, true);
}

return nll;
}"

inits <- list(bIntercept = 0, bWt = 0, bHp = 0, bDisp = 0, bDisp2 = 0, log_sigma = 0)

model <- tmb_model(model_code, inits = inits, scale = c("wt", "hp", "disp"))

analysis <- analyse(model, data = mtcars)

tidy(analysis)
#>         term  estimate std.error statistic       p.value      lower
#> 1 bIntercept 18.483095 0.5763207 32.070848 1.124454e-225 17.3535270
#> 2        bWt -3.633191 0.8222207 -4.418754  9.927147e-06 -5.2447139
#> 3        bHp -1.450231 0.6461069 -2.244568  2.479586e-02 -2.7165772
#> 4      bDisp -1.388636 1.0699325 -1.297872  1.943312e-01 -3.4856651
#> 5     bDisp2  1.659380 0.4582492  3.621131  2.933177e-04  0.7612285
#> 6  log_sigma  0.731897 0.1249999  5.855180  4.764933e-09  0.4869017
#>        upper
#> 1 19.6126628
#> 2 -2.0216681
#> 3 -0.1838848
#> 4  0.7083933
#> 5  2.5575323
#> 6  0.9768923
glance(analysis)
#>      logLik
#> 1 -68.82675

analysis2 <- lm(mpg ~ wt + hp + poly(disp,2), 
        data = rescale::rescale(mtcars, scale = c("wt", "hp", "disp")))

tidy(analysis2)
#>             term  estimate std.error  statistic      p.value
#> 1    (Intercept) 20.090625 0.4001079 50.2130198 3.336645e-28
#> 2             wt -3.633150 0.8951208 -4.0588374 3.787112e-04
#> 3             hp -1.450218 0.7033923 -2.0617489 4.897818e-02
#> 4 poly(disp, 2)1 -4.091950 6.2124626 -0.6586679 5.156872e-01
#> 5 poly(disp, 2)2  7.875696 2.3677784  3.3261962 2.546081e-03
glance(analysis2)
#>   r.squared adj.r.squared    sigma statistic      p.value df    logLik
#> 1  0.877168     0.8589707 2.263352  48.20313 6.521212e-12  5 -68.82675
#>        AIC      BIC deviance df.residual
#> 1 149.6535 158.4479 138.3146          27
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

Inspiration
-----------

-   [broom](https://github.com/dgrtwo/broom)
-   [jaggernaut](https://github.com/poissonconsulting/jaggernaut)

Documentation
-------------

-   [TMB](https://github.com/kaskr/adcomp)
