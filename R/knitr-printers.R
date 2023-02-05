#' Print Equations (via `knit_print`)
#'
#' Converts a base R object into LaTeX output within Quarto / RMD.
#'
#' @template knitr-printer
#' @param x object to be passed to [`to_latex`].
#'
#' @section Chunk Options:
#'   - `eqn_name` character. Name to place on the LHS of the equation.
#'   - `eqn_digits` integer. Passed to [`to_latex`].
#'   - `eqn_envir` character. Passed to [`to_latex`].
#'
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


#' Print Tabsets (via `knit_print`)
#'
#' Converts a list to a Quarto / RMD tabset using the names as tab headings.
#'
#' @template knitr-printer
#' @param x list. Each element is passed to `knit_print` within tabs.
#'
#' @export
printer_tabset <- function(x, options, ...) {
  if (is.null(names(x))) names(x) <- seq_along(x)

  # Backwards compatibility with .Rmd
  is_rmd <- grepl("\\.Rmd$", knitr::current_input(), ignore.case = TRUE)

  if (isTRUE(is_rmd)) {
    header <- "#### { .tabset .unlisted .unnumbered}"
    footer <- "#### {.unlisted .unnumbered}"
  } else {
    header <- ":::: {.panel-tabset}"
    footer <- "::::"
  }

  tabs <-
    paste(
      sprintf("##### %s\n", names(x)),
      "```{r, echo = FALSE}",
      sprintf("x[[%i]]", seq_along(x)),
      "```",
      sep = "\n",
      collapse = "\n\n"
    )

  out <- knitr::knit_child(
    text = c(header, tabs, footer),
    options = options[c("fig.asp", "fig.width", "opts.label")],
    envir = environment(),
    quiet = TRUE
  )

  return(knitr::asis_output(out))
}
