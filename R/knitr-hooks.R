#' Knit (Chunk) Hooks
#'
#' Knit (chunk) hooks allow code to be ran before and after the execution of
#'   code chunks, see \url{https://yihui.org/knitr/hooks} for more information.
#'
#' @usage knitr::knit_hooks$set(NAME = knit_hook_*)
#'
#' @param before,options,envir required arguments.
#'
#' @name knit-hook-chunk
NULL

#' Summary, Hide Content (Knit Hook)
#'
#' @describeIn knit-hook-chunk
#'   Hide content inside of \code{<details><summary> ...} where the summary text
#'   is the value of the chunk option.
#'
#' @export
knit_hook_summary <- function(before, options, envir) {
  if (before) {
    stopifnot(knitr::is_html_output())
    pre_text <- sprintf("<details><summary>%s</summary>\n", options$summary)
    knitr::asis_output(pre_text)
  } else {
    knitr::asis_output("</details>")
  }
}
