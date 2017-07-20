
<!-- README.md is generated from README.Rmd. Please edit that file -->
![stability-unstable](https://img.shields.io/badge/stability-unstable-yellow.svg) [![Travis-CI Build Status](https://travis-ci.org/poissonconsulting/tmbr.svg?branch=master)](https://travis-ci.org/poissonconsulting/tmbr) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/poissonconsulting/tmbr?branch=master&svg=true)](https://ci.appveyor.com/project/poissonconsulting/tmbr) [![codecov](https://codecov.io/gh/poissonconsulting/smbr/branch/master/graph/badge.svg)](https://codecov.io/gh/poissonconsulting/smbr) [![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)

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
library(magrittr)
library(ggplot2)
library(tmbr)
```

``` r
model <- model("#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() () {

DATA_VECTOR(Pairs);
DATA_VECTOR(Year);
DATA_FACTOR(Annual);
DATA_INTEGER(nAnnual);

PARAMETER(alpha);
PARAMETER(beta1);
PARAMETER(beta2);
PARAMETER(beta3);
PARAMETER_VECTOR(bAnnual);
PARAMETER(log_sAnnual);

Type sAnnual = exp(log_sAnnual);

vector<Type> ePairs = Pairs;

Type nll = 0.0;

for(int i = 0; i < nAnnual; i++){
  nll -= dnorm(bAnnual(i), Type(0), sAnnual, true);
}

for(int i = 0; i < Pairs.size(); i++){
  ePairs(i) = exp(alpha + beta1 * Year(i) + beta2 * pow(Year(i), 2) + beta3 * pow(Year(i), 3) + bAnnual(Annual(i)));
  nll -= dpois(Pairs(i), ePairs(i), true);
}
ADREPORT(sAnnual)
return nll;
}")

# add R code to calculate derived parameters
model %<>% update_model(new_expr = "
for (i in 1:length(Pairs)) {
  log(prediction[i]) <- alpha + beta1 * Year[i] + beta2 * Year[i]^2 + beta3 * Year[i]^3 + bAnnual[Annual[i]]
}")

# define data types and center year
model %<>% update_model(
  gen_inits = function(data) list(alpha = 4, beta1 = 1, beta2 = 0, beta3 = 0, log_sAnnual = 0, bAnnual = rep(0, data$nAnnual)),
  select_data = list("Pairs" = integer(), "Year*" = integer(), Annual = factor()),
  random_effects = list(bAnnual = "Annual"))

data <- bauw::peregrine
data$Annual <- factor(data$Year)

analysis <- analyse(model, data = data)
#> Note: Using Makevars in /Users/joe/.R/Makevars 
#> # A tibble: 1 x 6
#>       n     K    logLik       IC       duration converged
#>   <int> <int>     <dbl>    <dbl> <S4: Duration>     <lgl>
#> 1    40     5 -154.4664 320.6974           1.2s      TRUE
#> Warning: 4 external pointers will be removed

coef(analysis)
#> # A tibble: 5 x 7
#>          term    estimate         sd     zscore       lower      upper
#>    <S3: term>       <dbl>      <dbl>      <dbl>       <dbl>      <dbl>
#> 1       alpha  4.21204711 0.03885239 108.411527  4.13589782  4.2881964
#> 2       beta1  1.19085200 0.06944561  17.147982  1.05474111  1.3269629
#> 3       beta2  0.01719698 0.02984903   0.576132 -0.04130604  0.0757000
#> 4       beta3 -0.27161560 0.03566621  -7.615488 -0.34152009 -0.2017111
#> 5 log_sAnnual -2.30854794 0.27060756  -8.530981 -2.83892901 -1.7781669
#> # ... with 1 more variables: pvalue <dbl>
```

``` r
year <- predict(analysis, new_data = "Year")
#> Warning in bind_rows_(x, .id): Vectorizing 'term' elements may not preserve
#> their attributes

#> Warning in bind_rows_(x, .id): Vectorizing 'term' elements may not preserve
#> their attributes

#> Warning in bind_rows_(x, .id): Vectorizing 'term' elements may not preserve
#> their attributes

ggplot(data = year, aes(x = Year, y = estimate)) +
  geom_point(data = bauw::peregrine, aes(y = Pairs)) +
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
