context("utils")

test_that("is.named", {
  expect_true(is.named(c(x = 1)))
  expect_false(is.named(c(1)))
  expect_true(is.named(list(x = 1)))
  expect_false(is.named(list(1)))
})

test_that("is.named_list", {
  expect_false(is.named_list(c(x = 1)))
  expect_false(is.named_list(c(1)))
  expect_true(is.named_list(list(x = 1)))
  expect_false(is.named_list(list(1)))
})

test_that("dims", {
  expect_identical(dims(1), 1L)
  expect_identical(dims(1:2), 2L)
  expect_identical(dims(matrix(1:2)), c(2L:1L))
})
