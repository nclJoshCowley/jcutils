#' @param options,... arguments required for a [`knit_print`][knitr::knit_print]
#'   method.
#'
#' @family Knitr printer methods
#'
#' @section Reporting:
#'   Either define a method, say `knit_print.foo <- printer_*`, in the report
#'   or set the `render` chunk option equal to the desired function.
#'
#' @seealso
#'   - Generic documentation, [`knit_print`][knitr::knit_print].
#'   - Detailed vignette, `vignette("knit_print", package = "knitr")`.
#'   - Related [`printr`][printr::printr] package.
