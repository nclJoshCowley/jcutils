#' Output List Elements as Tabset
#'
#' Map elements through `.f` and wrap each output into a tab within a tabset.
#'
#' @param .x list to be looped over, names inform tab headings.
#' @param .f function or formula. See [purrr::as_mapper()].
#' @param type choice, Quarto or R Markdown. Which rendering program is used?
#' @param ... extra arguments passed to `.f`
#'
#' @return `.x` is silently returned to allow for piping.
#'
#' @export
knitr_tabset <- function(.x, .f, type = c("quarto", "rmd"), ...) {
  lifecycle::deprecate_soft("v0.3.0", "knitr_print_tabset()", "printer_tabset()")

  if (missing(.f)) .f <- print
  .f <- rlang::as_function(.f, ...)

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

  cat("\n\n", header, "\n\n", sep = "")

  for (i in seq_along(.x)) {
    cat("##### ", nms[i], "\n\n", sep = "")
    .f(.x[[i]], ...)
    cat("\n\n")
  }

  cat(footer)

  invisible(.x)
}


#' Nice Table in R Markdown.
#'
#' This function utilises the flexibility of kableExtra to choose some 'nice'
#'   default tables for either HTML or PDF via LaTeX. (subjective)
#'
#' @param x data.frame. Data to be converted into a table.
#' @param fmt choice. Is the table to be displayed via HTML ("html")
#'   or via PDF ("latex").
#' @param scroll_vert numeric. If the format is HTML, create a scroll box of this
#'   height (in pixels)
#' @param ... extra arguments to be passed to either `kableExtra::kbl`
#'   or `kableExtra::kable_styling`
#'
#' @section Future Development:
#' - Add in some method to alter column widths, likely as a ncol-length simplex.
#'
#' @return Either a HTML table or LaTeX table, depending on format.
#'
#' @export
rmd_table <- function(x, fmt = c("html", "latex"), scroll_vert = NULL, ...) {
  lifecycle::deprecate_warn("v0.3.0", "rmd_table()", "gt::gt()")

  # Parse arguments
  fmt <- match.arg(fmt)
  dots <- list(...)

  # Validation
  stopifnot("'x' should inherit from data.frame" = inherits(x, "data.frame"))
  if (!is.null(scroll_vert)) {
    if (fmt == "latex") stop("Scroll box in HTML only.")
    if (!is.numeric(scroll_vert)) stop("Scroll box argument should be numeric.")
  }

  # Obtain (modified) default arguments
  kbl_args <- formals(kableExtra::kbl)
  kbl_args$x <- quote(x)
  kbl_args$format <- fmt
  kbl_args$digits <- 3
  kbl_args$row.names <-  FALSE
  kbl_args$align <- "c"
  kbl_args$... <- NULL
  if (fmt == "latex") {
    kbl_args$booktabs <- TRUE
    kbl_args$linesep <- ""
    kbl_args$table.envir <- "table"
  }

  kbl_args_with_dots <- c(
    kbl_args[!names(kbl_args) %in% names(dots)],
    dots[names(dots) %in% names(kbl_args)]
  )

  # Obtain (modified) default style arguments
  kbl_styles <- formals(kableExtra::kable_styling)
  kbl_styles$bootstrap_options <- c("striped", "hover", "responsive")
  if (fmt == "latex") kbl_styles$latex_options <- c("striped", "HOLD_position")
  kbl_styles$full_width <- TRUE
  if (fmt == "html") kbl_styles$fixed_thead <- TRUE

  kbl_styles_with_dots <- c(
    kbl_styles[!names(kbl_styles) %in% names(dots)],
    dots[names(dots) %in% names(kbl_styles)]
  )

  # Call to kbl() with relevant arguments
  call_kbl <- as.call(c(
    list(quote(kableExtra::kbl)),
    kbl_args_with_dots
  ))
  kbl_styles_with_dots$kable_input <- call_kbl

  # Call to kable_styling() with relevant arguments
  call_kblstyle <- as.call(c(
    list(quote(kableExtra::kable_styling)),
    kbl_styles_with_dots
  ))


  # Return with option scroll box (vertical only)
  out <- eval(call_kblstyle, rlang::env(booktabs = TRUE))
  if (!is.null(scroll_vert)) {
    kableExtra::scroll_box(out, width = "100%", height = paste0(scroll_vert, "px"))
  } else {
    out
  }
}
