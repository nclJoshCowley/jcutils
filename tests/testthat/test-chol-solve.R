context("Invert matrices with cholesky solve")

test_that("chol_solve and solve consistent", {
  x_test <- matrix(
    c(20, 5, 8.5, 5, 17, 10.5, 8.5, 10.5, 19),
    nrow = 3,
    ncol = 3
  )

  expect_equal(
    solve(x_test),
    SiteDataCollection::chol_solve(x_test)
  )
})

test_that("chol_solve refuses asymmetric matrices", {
  expect_error(
    SiteDataCollection::chol_solve(matrix(1:4, 2, 2))
  )
})

test_that("chol_solve refuses matrices with neagtive eigenvalues", {
  expect_error(
    SiteDataCollection::chol_solve(diag(c(-1, 1, 2)))
  )
})
