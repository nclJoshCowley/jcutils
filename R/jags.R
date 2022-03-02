#' Knitr Friendly Version of `jags.samples()`
#'
#' Modified version of \code{\link[rjags]{jags.samples}} that incorporates
#'   [https://rmflight.github.io/knitrProgressBar/]{knitrProgressBar}.
#'
#' @param type Argument not implemented, default "trace" assumed.
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
jags_samples_in_knitr <- function(model, variable.names, n.iter, thin) {
  # Validation
  stopifnot(
    "Invalid JAGS model" =
      inherits(model, "jags"),

    "variable.names must be a character vector" =
      (is.character(variable.names) & length(variable.names) > 0),

    "n.iter must be a positive integer" =
      (is.numeric(n.iter) & (length(n.iter) == 1) & (n.iter > 0)),

    "thin must be a positive integer" =
      (is.numeric(thin) & (length(thin) == 1) & (thin > 0))
  )

  startiter <- model$iter()
  n.iter <- n.iter - n.iter %% thin

  # Initialise progress bar to work in knitr
  steps <- n.iter / thin
  pb <- knitrProgressBar::progress_estimated(steps + 1)
  knitrProgressBar::update_progress(pb)

  # Code simplified by only allowing type = "trace"
  type <- "trace"
  pn <- parse.varnames(variable.names)

  # Set up monitors
  status <- .Call("set_monitors", model$ptr(), pn$names, pn$lower, pn$upper,
                  as.integer(thin), type, PACKAGE = "rjags")
  if (!any(status)) stop("No valid monitors set")

  # Update per step
  for (. in seq_len(steps)) {
    stats::update(model, n.iter / steps, progress.bar = "none")
    knitrProgressBar::update_progress(pb)
  }

  # Obtain and post-process monitors
  ans <- .Call("get_monitored_values", model$ptr(), type, PACKAGE = "rjags")

  for (i in seq_along(ans)) {
    class(ans[[i]]) <- "mcarray"
    attr(ans[[i]], "varname") <- names(ans)[i]

    # Assure there is a valid dim attribute for pooled scalar nodes:
    if(is.null(dim(ans[[i]]))) dim(ans[[i]]) <- length(ans[[i]])

    # New attributes for rjags_4-7:
    attr(ans[[i]], "type") <- type
    attr(ans[[i]], "iterations") <-
      c(
        start = startiter + thin,
        end = startiter + n.iter,
        thin=thin
      )
  }

  # Clear any monitors
  for (i in seq_along(variable.names)) {
    if (status[i]) {
      .Call("clear_monitor", model$ptr(), pn$names[i], pn$lower[[i]],
            pn$upper[[i]], type, PACKAGE = "rjags")
    }
  }

  # Return list of `mcarray` objects
  return(ans)
}


#' Parse Varnames
#'
#' Internal `rjags` function required for \code{\link{jags_samples_in_knitr}}.
#'
#' @keywords internal
parse.varnames <- utils::getFromNamespace("parse.varnames", "rjags")
