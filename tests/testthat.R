library(testthat)
library(tmbr)

Sys.setenv("R_TESTS" = "")

test_check("tmbr")
