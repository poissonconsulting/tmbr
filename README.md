
<!-- README.md is generated from README.Rmd. Please edit that file -->
![stability-unstable](https://img.shields.io/badge/stability-unstable-yellow.svg) [![Travis-CI Build Status](https://travis-ci.org/poissonconsulting/tmbr.svg?branch=master)](https://travis-ci.org/poissonconsulting/tmbr) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/poissonconsulting/tmbr?branch=master&svg=true)](https://ci.appveyor.com/project/poissonconsulting/tmbr) [![codecov](https://codecov.io/gh/poissonconsulting/tmbr/branch/master/graph/badge.svg)](https://codecov.io/gh/poissonconsulting/tmbr) [![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)

tmbr
====

Introduction
------------

`tmbr` (pronounced timber) is an R package to facilitate analyses using Template Model Builder (TMB). It is part of the [mbr](https://github.com/poissonconsulting/mbr) family of packages.

Installation
------------

Installation of TMB on Windows is currently proving [challenging](https://github.com/James-Thorson/2016_Spatio-temporal_models/issues/7). Until these issues are resolved `tmbr` is only supported on unix-based OSs.

To install from GitHub

    # install.packages("devtools")
    devtools::install_github("poissonconsulting/tmbr")

Demonstration
-------------

``` r
library(tmbr)
```

``` r
options("mb.parallel" = TRUE)
doParallel::registerDoParallel(4)

data <- bauw::peregrine

template <- "
#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() () {

DATA_VECTOR(Pairs);
DATA_VECTOR(Year);

PARAMETER(alpha);
PARAMETER(beta1);
PARAMETER(beta2);
PARAMETER(beta3);

vector<Type> ePairs = Pairs;

Type nll = 0.0;

for(int i = 0; i < Pairs.size(); i++){
  ePairs(i) = exp(alpha + beta1 * Year(i) + beta2 * pow(Year(i), 2) + beta3 * pow(Year(i), 3));
  nll -= dpois(Pairs(i), ePairs(i), true);
}
return nll;
}"

new_expr <- "
for (i in 1:length(Pairs)) {
  prediction[i] <- exp(alpha + beta1 * Year[i] + beta2 * Year[i]^2 + beta3 * Year[i]^3)
}"

gen_inits <- function(data) list(alpha = 4, beta1 = 1, beta2 = 0, beta3 = 0)

model <- model(template, scale = "Year", gen_inits = gen_inits, new_expr = new_expr)

analysis <- analyse(model, data = data)
#> Note: Using Makevars in /Users/joe/.R/Makevars 
#> # A tibble: 1 × 6
#>       n     K    logLik     AICc          duration converged
#>   <int> <int>     <dbl>    <dbl>    <S4: Duration>     <lgl>
#> 1    40     4 -159.1842 327.5113 0.10505485534668s      TRUE
#> Warning: 2 external pointers will be removed

coef(analysis)
#> # A tibble: 4 × 7
#>         term     estimate         sd     zscore       lower      upper
#>   <S3: term>        <dbl>      <dbl>      <dbl>       <dbl>      <dbl>
#> 1      alpha  4.232280328 0.03004250 140.876456  4.17339812  4.2911625
#> 2      beta1  1.116410020 0.04777944  23.365909  1.02276404  1.2100560
#> 3      beta2  0.007065185 0.02408137   0.293388 -0.04013343  0.0542638
#> 4      beta3 -0.233904157 0.02513417  -9.306221 -0.28316623 -0.1846421
#> # ... with 1 more variables: pvalue <dbl>

year <- predict(analysis, new_data = new_data(data, "Year"))
```

``` r
library(ggplot2)

ggplot(data = year, aes(x = Year, y = estimate)) +
  geom_point(data = data, aes(y = Pairs)) +
  geom_line() +
  expand_limits(y = 0)
```

![](tools/README-unnamed-chunk-4-1.png)

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
