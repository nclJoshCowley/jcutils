#' List to HTML tabset
#'
#' Evaluates elements of a named list using names add heading set as some
#'   chosen level (for use in `rmarkdown` documents).
#'
#' @param x named list.
#' @param level integer. How many `#` should precede each heading title.
#' @param id character. String to be used in chunk labelling, useful to avoid
#'   duplicate chunk labels.
#'
#' @export
list_to_html_tabset <- function(x, level, id = "id") {
  if (!knitr::is_html_output()) {
    warning("Not HTML output, exiting early")
    return(invisible(NULL))
  }

  if (!is.list(x) | is.null(names(x))) stop("'x' should be a named list")
  arg_name <- deparse(substitute(x))

  if (missing(level)) stop("Heading 'level' argument required")
  hn <- paste(rep("#", level), collapse = "")

  src <-
    lapply(names(x), function(.nm) {
      knitr::knit_expand(
        text = paste(
          "{{hn}} {{.nm}} \n",
          "```{r tabs-{{id}}-{{arg_name}}-{{.nm}}}",
          "x$`{{.nm}}`",
          "``` \n\n",
          sep = "\n"
        )
      )
    })

  res <-
    knitr::knit_child(
      text = unlist(src),
      envir = rlang::current_env(),
      quiet = TRUE
    )

  cat(res, sep = "\n")
}
