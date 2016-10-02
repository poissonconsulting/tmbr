context("utils")

test_that("is_named", {
  expect_true(is_named(c(x = 1)))
  expect_false(is_named(c(1)))
  expect_true(is_named(list(x = 1)))
  expect_false(is_named(list(1)))
})

test_that("is_named_list", {
  expect_false(is_named_list(c(x = 1)))
  expect_false(is_named_list(c(1)))
  expect_true(is_named_list(list(x = 1)))
  expect_false(is_named_list(list(1)))
})

test_that("dims", {
  expect_identical(dims(1), 1L)
  expect_identical(dims(1:2), 2L)
  expect_identical(dims(matrix(1:2)), c(2L:1L))
})
