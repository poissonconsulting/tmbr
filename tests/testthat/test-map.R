context("map")

test_that("map", {
  expect_identical(map(list()), list())
  expect_identical(map(list(x = c(1,2))), list())
  expect_identical(map(list(x = c(1,2,NA))), list(x = factor(c(1,2,NA))))
  expect_identical(map(list(y = 1:4, x = c(1,2,NA))), list(x = factor(c(1,2,NA))))
  expect_identical(map(list(y = 1:4, x = matrix(c(1,2,NA,4), nrow = 2))), list(x = factor(c(1,2,NA,4))))
})
