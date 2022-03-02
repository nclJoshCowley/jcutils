test_that("Simple JAGS example, thin = 1", {
  out <- expect_error(
    jcutils::jags_samples_in_knitr(model, variable.names, n.iter, thin = 1),
    regexp = NA
  )

  expect_equal(
    dim(out$mu),
    c(1, iteration = n.iter, chain = 2)
  )

  expect_equal(
    dim(out$pred_matrix),
    c(2, 10, iteration = n.iter, chain = 2)
  )

})


test_that("Simple JAGS example, thin = 3", {
  out <- expect_error(
    jcutils::jags_samples_in_knitr(model, variable.names, n.iter, thin = 3),
    regexp = NA
  )

  thinned_iter <- floor(n.iter / 3)

  expect_equal(
    dim(out$mu),
    c(1, iteration = thinned_iter, chain = 2)
  )

  expect_equal(
    dim(out$pred_matrix),
    c(2, 10, iteration = thinned_iter, chain = 2)
  )

})
