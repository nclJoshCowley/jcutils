#' @param options,... arguments required for a [`knit_print`][knitr::knit_print]
#'   method.
#'
#' @family Knitr printer methods
#'
#' @section Reporting:
#'   These methods are to be used in Quarto / RMD reports.
#'
#'   For global usage on all objects of class `foo`, register as an S3 method,
#'   typically in the setup chunk.
#'
#'   ```r
#'   library(knitr)
#'   registerS3method("knit_print", "foo", printer_*)
#'   ```
#'
#'   For one-off usage, set the `render` chunk option, that is:
#'   ````markdown
#'   ```{r}
#'   #| render: !expr printer_*
#'
#'   ```
#'   ````
#'
#' @seealso
#'   - Generic documentation, [`knit_print`][knitr::knit_print].
#'   - Detailed vignette, `vignette("knit_print", package = "knitr")`.
#'   - Related [`printr`][printr::printr] package.
