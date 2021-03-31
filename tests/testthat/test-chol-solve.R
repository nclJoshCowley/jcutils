context("Invert matrices with cholesky solve")

x_test <- structure(
  c(20, 5, 8.5, 5, 17, 10.5, 8.5, 10.5, 19),
  .Dim = c(3L, 3L)
)

test_that("chol_solve and solve consistent", {
  expect_equal(solve(x_test), jcutils::chol_solve(x_test))
})

test_that("chol_solve refuses asymmetric matrices", {
  expect_error(jcutils::chol_solve(matrix(1:4, 2, 2)))
})

test_that("chol_solve refuses matrices with neagtive eigenvalues", {
  expect_error(jcutils::chol_solve(diag(c(-1, 1, 2))))
})
