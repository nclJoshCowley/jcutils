#' Output List Elements as Tabset
#'
#' Map elements through `.f` and wrap each output into a tab within a tabset.
#'
#' @param .x list to be looped over, names inform tab headings.
#' @param .f function or formula. See \code{\link[purrr]{as_mapper}}.
#' @param type choice, Quarto or R Markdown. Which rendering program is used?
#' @param ... extra arguments passed to `.f`
#'
#' @return `.x` is silently returned to allow for piping.
#'
#' @export
knitr_tabset <- function(.x, .f, type = c("quarto", "rmd"), ...) {
  if (missing(.f)) .f <- print
  .f <- purrr::as_mapper(.f, ...)

  nms <- if (is.null(names(.x))) seq_along(.x) else names(.x)

  header <-
    switch(
      match.arg(type),
      quarto = ":::: {.panel-tabset}",
      rmd = "#### { .tabset .unlisted .unnumbered}"
    )

  footer <-
    switch(
      match.arg(type),
      quarto = "::::",
      rmd = "#### {.unlisted .unnumbered}"
    )

  cat(header, "\n\n", sep = "")

  for (i in seq_along(.x)) {
    cat("##### ", nms[i], "\n\n", sep = "")
    .f(.x[[i]], ...)
    cat("\n\n")
  }

  cat(footer)

  invisible(.x)
}


#' @describeIn knitr_tabset Deprecated, use `knitr_tabset(x, print, "rmd")`.
#' @export
knitr_print_tabset <- function(x, level) {
  lifecycle::deprecate_soft("v0.3.0", "knitr_print_tabset()", "knitr_tabset()")
  knitr_tabset(.x = x, .f = print, type = "rmd")
}
