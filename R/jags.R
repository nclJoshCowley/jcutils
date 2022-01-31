#' JAGS Samples with Progress Bar
#'
#' \code{\link[rjags]{jags.samples}} default progress bar will not work when
#'   `interactive()` returns `FALSE`. This method uses the `knitrProgressBar`
#'   package with many `jags.samples()` calls.
#'
#' @inheritParams rjags::jags.samples
#' @param n.update integer. Number of (unsaved) warn-up iterations.
#'
#' @export
jags_samples_with_pb <- function(model, variable.names, n.iter, n.update, thin) {
  # Define **alternate** progress bar
  steps <- min(100, n.iter)
  pb <- knitrProgressBar::progress_estimated(steps)

  # Define trace start and end points
  ends <- cumsum(rep(floor(n.iter / steps), steps))
  ends[length(ends)] <- n.iter
  starts <- 1 + c(0, utils::head(ends, -1))

  # Run the sampler to get dimensions
  warmup_samples <-
    rjags::jags.samples(
      model,
      variable.names,
      thin,
      n.iter = 100,
      type = "trace",
      quiet = TRUE,
      progress.bar = "none"
    )
  vardims <- purrr::map(warmup_samples, ~ utils::head(dim(.x), -2))
  rm(warmup_samples)

  # Output to be same as usual jags.samples() call
  out <- list()
  for (varnm in variable.names) {
    dims_attr <-
      c(
        vardims[[varnm]],
        iteration = n.iter,
        chain = model$nchain()
      )

    iterations_attr <-
      c(
        start = model$iter() + starts[1],
        end = model$iter() + ends[length(ends)],
        thin = thin
      )

    out[[varnm]] <-
      structure(
        array(data = NA, dim = dims_attr),
        class = "mcarray",
        varname = varnm,
        type = "trace",
        iterations = iterations_attr
      )
  }

  # Loop through
  for(i in seq_along(starts)) {
    # Run JAGS
    cur_samples <-
      rjags::jags.samples(
        model,
        variable.names,
        thin,
        n.iter = (ends[i] + 1) - starts[i],
        type = "trace",
        quiet = TRUE,
        progress.bar = "none"
      )

    # Save into object to be output
    for (varnm in names(out)) {
      out[[varnm]] <-
        assign_iterations_mcarray(
          out[[varnm]],
          cur_samples[[varnm]],
          at = seq(starts[i], ends[i])
        )
    }

    # Update progress bar
    knitrProgressBar::update_progress(pb)
  }

  return(out)
}


#' Assign iterations mcarray
#'
#' Assign values to an mcarray object. Note this is not straightforward because
#'   for a scalar one would use `x[, 1:10, ] <- value`, whereas for a matrix
#'   one would use `x[, , 1:10, ] <- value`.
#'
#' @param x mcarray object.
#' @param value mcarray object with same dimension except for the penultimate
#'   dimension (iterations).
#' @param at vector of integer values at which to place the `value` array.
#'
#' @examples \dontrun{
#'   x <- array(seq(180), dim = c(2, 2, 15, 3))
#'   value <- 1000 * x[, , 6:10, ]
#'   at <- 6:10
#'   assign_iterations_mcarray(x, value, at)
#' }
#'
#' @export
assign_iterations_mcarray <- function(x, value, at) {
  dx <- dim(x)
  dv <- dim(value)

  nx_iter <- dx[length(dx) - 1]
  nx_chains <- dx[length(dx)]
  nx_vars <- prod(utils::head(dx, -2))

  nv_iter <- dv[length(dv) - 1]
  nv_chains <- dv[length(dv)]
  nv_vars <- prod(utils::head(dv, -2))

  stopifnot(
    "No. of chains incompatible" =
      all.equal(nx_chains, nv_chains, check.names = FALSE),

    "Variable dims incompatible" =
      all.equal(nx_vars, nv_vars, check.names = FALSE),

    "Replacement length not equal to 'at'" =
      all.equal(length(at), nv_iter, check.names = FALSE)
  )

  x_flat <- structure(x, dim = c(nx_vars, nx_iter, nx_chains))
  value_flat <- structure(value, dim = c(nv_vars, nv_iter, nv_chains))

  x_flat[, at, ] <- value_flat

  return(structure(x_flat, dim = dx))
}
