test_that("Simple JAGS example throws no errors, thin = 1", {
  expect_error(
    regexp = NA,
    object = jags_samples_in_knitr(model, variable.names, n.iter, thin = 1)
  )
})
