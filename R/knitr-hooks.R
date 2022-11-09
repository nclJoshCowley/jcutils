#' Knit (Chunk) Hooks
#'
#' Knit (chunk) hooks to be ran before and after the execution of code chunks.
#'
#' @section Knitr:
#'   To use any of these functions. use the following in a R markdown document:
#'   ```
#'   knitr::knit_hooks$set(NAME = knit_hook_*)
#'   ```
#'   See <https://yihui.org/knitr/hooks> for more information.
#'
#' @param before,options,envir required arguments.
#'
#' @name knit-hook-chunk
NULL

#' Summary, Hide Content (Knit Hook)
#'
#' @describeIn knit-hook-chunk
#'   Hide content inside of `<details><summary> ...` where the summary text
#'   is the value of the chunk option.
#'
#' @export
knit_hook_summary <- function(before, options, envir) {
  if (before) {
    pre_text <- sprintf("<details><summary>%s</summary>\n", options$summary)
    knitr::asis_output(pre_text)
  } else {
    knitr::asis_output("</details>")
  }
}
