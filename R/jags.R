#' Knitr Friendly Version of `jags.samples()`
#'
#' Modified version of \code{\link[rjags]{jags.samples}} that incorporates
#'   [https://rmflight.github.io/knitrProgressBar/]{knitrProgressBar}.
#'
#' @inheritParams rjags::jags.samples
#'
#' @details
#'   In the source code of \code{\link[rjags]{jags.samples}} and the
#'   \code{\link[rjags]{update}}, a progress bar is called only if
#'   `interactive()` returns `TRUE`.
#'
#'   This is not the case for Rmarkdown documents among other scenarios, hence
#'   we modify the source code to display a different progress bar.
#'
#' @seealso \code{\link[rjags]{jags.samples}} and its source code.
#'
#' @export
jags_samples_in_knitr <- function(model, variable.names, n.iter,
                                  thin = 1, type = "trace", force.list = FALSE,
                                  ...) {

  # Create a copy of jags.samples(), with an *editable* (child) environment
  sub_env <- rlang::new_environment(parent = rlang::ns_env("rjags"))
  jags.samples_copy <- rjags::jags.samples
  environment(jags.samples_copy) <- sub_env

  # Replace update method with progress bar wrapper version
  sub_env$update.jags <- function(model, n.iter, progress.bar, ...) {
    steps <- n.iter / rlang::caller_env()$thin
    pb <- knitrProgressBar::progress_estimated(steps)

    for (. in seq_len(steps)) {
      stats::update(model, n.iter / steps, progress.bar = "none", ...)
      knitrProgressBar::update_progress(pb)
    }
  }

  # Call the resultant copy and return the output
  jags.samples_copy(model, variable.names, n.iter, thin)
}


#' Notebook Friendly Version of `jags.samples()`
#'
#' Modified version of \code{\link[rjags]{jags.samples}} that incorporates
#'   [https://rmflight.github.io/knitrProgressBar/]{knitrProgressBar}.
#'
#' @param ... passed to \code{\link[rjags]{jags.samples}}.
#'
#' @export
jags_samples_quarto <- function(...) {
  jags_samples <- rjags::jags.samples

  environment(jags_samples) <- new.env(parent = rlang::ns_env("rjags"))

  environment(jags_samples)$update.jags <- function(object, n.iter, ...) {
    # Create 100-length vector relaying n.iter for each update
    sub_iters <- unname(table(cut(seq_len(n.iter), 100)))

    if (n.iter < 100) sub_iters <- rep(1, n.iter)

    pb <- knitrProgressBar::progress_estimated(length(sub_iters))

    for (sub_iter in sub_iters) {
      stats::update(object, n.iter = sub_iter, progress.bar = "none", ...)
      knitrProgressBar::update_progress(pb)
    }
  }

  jags_samples(...)
}
