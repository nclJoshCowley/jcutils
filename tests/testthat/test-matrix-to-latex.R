context("array to matrix")

test_that("array to matrix runs without errors", {
  x_num <- matrix(rnorm(8), 4, 2)
  x_char <- matrix(letters[1:8], 4, 2)

  expect_error(jcutils::matrix_to_latex(x_num), NA)
  expect_error(jcutils::matrix_to_latex(x_char), NA)
})
