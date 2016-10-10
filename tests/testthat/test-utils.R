context("utils")

test_that("replace_values", {
  expect_identical(replace_values("Year", list(Year = 1)), "1")
  expect_identical(replace_values("Year2", list(Year = 1)), "Year2")
  expect_identical(replace_values("Year[Year]", list(Year = 1)), "1[1]")
  expect_identical(replace_values("Year[Year,Year2]", list(Year = 1,Year2 = 2)), "1[1,2]")
})

test_that("parse_string", {
  expect_identical(parse_string("bYear * Year"), list(c("bYear", "Year")))
  expect_identical(parse_string(" bYear*Year "), list(c("bYear", "Year")))
  expect_identical(parse_string(" bYear*Year+Year "), list(c("bYear", "Year"), "Year"))
  expect_identical(parse_string(" bYear*Year+ "), list(c("bYear", "Year"), ""))
  expect_identical(parse_string(" bYear[1,2]*2+3*bThing[x,x]*        zz+*"), list(c("bYear[1,2]", "2"), c("3","bThing[x,x]", "zz"), c("", "")))
})

test_that("check_profile_expr", {
  expect_identical(check_profile_expr("bYear * Year"), "bYear * Year")
  expect_identical(check_profile_expr("bYear[1] * Year"), "bYear[1] * Year")
  expect_error(check_profile_expr("bYear[1] * (Year + 2)"), "profile_expr contains round brackets")
  expect_identical(check_profile_expr("bYear[1] * Year + 2"), "bYear[1] * Year + 2")
  expect_error(check_profile_expr("bYear[1] * Year + 2 +"), "profile_expr is incomplete")
})

test_that("dims", {
  expect_identical(dims(1), 1L)
  expect_identical(dims(1:2), 2L)
  expect_identical(dims(matrix(1:2)), c(2L:1L))
})

test_that("list_by_name", {
  expect_identical(list_by_name(c(bYear = 1, bYear = 2)), list(bYear = c(1,2)))
})

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


