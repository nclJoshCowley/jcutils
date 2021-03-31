context("Testdata creation function")

test_that("Function runs with default arguments", {
  expect_error(jcutils::create_testdata(), NA)
})

test_that("incl_na = FALSE omits all missing data", {
  n_missing <- sum(is.na(jcutils::create_testdata(incl_na = FALSE)))
  expect_equal(n_missing, 0L)
})
