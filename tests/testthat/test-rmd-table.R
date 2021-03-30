context("RMD helper (Table)")

# Required
x <- jcutils::testdata
# Should be passed to kableExtra::kbl()
t_digits <- 5
# Should be passed to kableExtra::kable_styling()
t_fw <- FALSE


test_that("testdata table can be converted to HTML", {
  expect_error(jcutils::rmd_table(x, fmt = "html"), NA)
})

test_that("testdata table can be converted to PDF", {
  expect_error(jcutils::rmd_table(x, fmt = "latex"), NA)
})

test_that("function accepts arguments passed to kbl()", {
  expect_error(jcutils::rmd_table(x, fmt = "html", digits = t_digits), NA)
  expect_error(jcutils::rmd_table(x, fmt = "latex", digits = t_digits), NA)
})

test_that("function accepts arguments passed to kable_styling()", {
  expect_error(jcutils::rmd_table(x, fmt = "html", full_width = t_fw), NA)
  expect_error(jcutils::rmd_table(x, fmt = "latex", full_width = t_fw), NA)
})

test_that("Scroll bar in LaTeX throws error", {
  expect_error(jcutils::rmd_table(x, fmt = "latex", scroll_h = 200))
})
