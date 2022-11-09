#' Documentation upcoming
#' @export
printer_equation <- function(x, options, ...) {
  digits <- options$eqn_digits
  envir <- options$eqn_envir %||% "pmatrix"

  rhs <- to_latex(x, digits = digits, envir = envir)

  if (is.null(options$eqn_name) || nzchar(options$eqn_name) == 0) {
    body <-  rhs
  } else {
    body <- sprintf("%s = %s", options$eqn_name, rhs)
  }

  structure(paste("$$", body, "$$", sep = "\n"), class = "knit_asis")
}

