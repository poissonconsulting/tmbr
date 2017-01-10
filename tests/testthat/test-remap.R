context("remap")

test_that("remap_vector", {
  expect_identical(remap_vector(c(1,3), factor(c(1,NA,3))), c(1,0,3))
  expect_identical(remap_vector(c(1,3), factor(c(1,NA,3,NA))), c(1,0,3,0))
  expect_error(remap_vector(c(1,3), factor(c(1,NA))))
  expect_identical(remap_vector(c(1,3), factor(c(1,4))), c(1,3))
  expect_error(remap_vector(c(1,3), factor(c(1,4,5))))
})

test_that("remap", {
  expect_identical(remap_estimates(estimates = list(x = c(1,3)), map = list()), list(x = c(1,3)))
  expect_identical(remap_estimates(estimates = list(x = c(1,3)), map = list(x = factor(c(1,NA,3)))), list(x = c(1,0,3)))
})
