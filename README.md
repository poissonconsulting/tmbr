
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
#> 
#> Attaching package: 'tmbr'
#> The following object is masked from 'package:stats':
#> 
#>     profile

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

gen_inits <- function(data) list(bIntercept = 0, bWt = 0, bHp = 0, bDisp = 0, bDisp2 = 0, log_sigma = 0)

model <- tmb_model(model_code, gen_inits = gen_inits, scale = c("wt", "hp", "disp"))

analysis <- analyse(model, data = mtcars)

coef(analysis)
#> # A tibble: 6 × 7
#>         term  estimate std.error statistic       p.value      lower
#>        <chr>     <dbl>     <dbl>     <dbl>         <dbl>      <dbl>
#> 1 bIntercept 18.483095 0.5763207 32.070848 1.124454e-225 17.3535270
#> 2        bWt -3.633191 0.8222207 -4.418754  9.927147e-06 -5.2447139
#> 3        bHp -1.450231 0.6461069 -2.244568  2.479586e-02 -2.7165772
#> 4      bDisp -1.388636 1.0699325 -1.297872  1.943312e-01 -3.4856651
#> 5     bDisp2  1.659380 0.4582492  3.621131  2.933177e-04  0.7612285
#> 6  log_sigma  0.731897 0.1249999  5.855180  4.764933e-09  0.4869017
#> # ... with 1 more variables: upper <dbl>
logLik(analysis)
#> [1] -68.82675

analysis2 <- lm(mpg ~ wt + hp + poly(disp,2), 
        data = rescale::rescale(mtcars, scale = c("wt", "hp", "disp")))

summary(analysis2)
#> 
#> Call:
#> lm(formula = mpg ~ wt + hp + poly(disp, 2), data = rescale::rescale(mtcars, 
#>     scale = c("wt", "hp", "disp")))
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -3.3887 -1.6079 -0.3997  1.7992  4.2331 
#> 
#> Coefficients:
#>                Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)     20.0906     0.4001  50.213  < 2e-16 ***
#> wt              -3.6331     0.8951  -4.059 0.000379 ***
#> hp              -1.4502     0.7034  -2.062 0.048978 *  
#> poly(disp, 2)1  -4.0919     6.2125  -0.659 0.515687    
#> poly(disp, 2)2   7.8757     2.3678   3.326 0.002546 ** 
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 2.263 on 27 degrees of freedom
#> Multiple R-squared:  0.8772, Adjusted R-squared:  0.859 
#> F-statistic:  48.2 on 4 and 27 DF,  p-value: 6.521e-12
logLik(analysis)
#> [1] -68.82675
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
