#' JAGS Samples with Progress Bar
#'
#' \code{\link[rjags]{jags.samples}} default progress bar will not work when
#'   `interactive()` returns `FALSE`. This method uses the `knitrProgressBar`
#'   package with many `jags.samples()` calls.
#'
#' @inheritParams jags.samples
#' @param n.update integer. Number of (unsaved) warn-up iterations.
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

# Preamble
jags_data <-
  local({
    n <- 1000
    x <- stats::rnorm(n, 0, 5)
    list(n = n, x = x)
  })

jags_string <- "
    model {
      for (i in 1:n) {
        x[i] ~ dnorm(mu, tau)
      }

      matr[1, 1:2] = c(mu, 0)
      matr[2, 1:2] = c(0, tau)

      mu ~ dnorm(0, 1 / 1000)
      tau ~ dgamma(10, 1)
    }
  "

jags_model <-
  rjags::jags.model(
    file = textConnection(jags_string),
    data = jags_data,
    n.chains = 4,
    n.adapt = 100,
    quiet = TRUE
  )

# Example usage
z <-
  jags_samples(
    model = jags_model,
    variable.names = c("mu", "tau", "matr"),
    n.iter = 100000,
    n.update = 1000,
    thin = 1,
    steps = 100
  )
