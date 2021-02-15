test_that("add_report", {
  expect_identical(add_report(" ADREPORT(343)"), "REPORT(343);\nADREPORT(343)")
})
