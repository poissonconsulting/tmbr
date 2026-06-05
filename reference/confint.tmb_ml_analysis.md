# Confint

Confint

## Usage

``` r
# S3 method for class 'tmb_ml_analysis'
confint(
  object,
  parm = terms(object),
  level = getOption("mb.conf_level", 0.95),
  parallel = getOption("mb.parallel", FALSE),
  beep = getOption("mb.beep", FALSE),
  ...
)
```

## Arguments

- object:

  The tmb_ml_analysis object.

- parm:

  A character vector of the *terms* to get the confidence interval for.

- level:

  A number specifying the confidence level. By default 0.95.

- parallel:

  A flag indicating whether to using parallel backend provided by
  foreach.

- beep:

  A flag indicating whether to beep on completion of the analysis.

- ...:

  Addtional arguments passed to TMB::tmbprofile
