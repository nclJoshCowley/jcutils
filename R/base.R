#' Range Expander
#'
#' Modifies the output of \code{\link[base]{range}} to stretch or shrink.
#'   This is mainly useful for setting plot limits.
#'
#' @param ... passed to \code{\link[base]{range}}.
#' @param scaling numeric. Scaling factor, will stretch when greater than 1
#'   and shrink when less than 1.
#'
#' @return numeric of length 2. Consider `x = 0:10`, (spans 10 units),
#'   - `range_expander(x, scaling = 0.5)` returns `c(2.5, 7.5)` (5 units),
#'   - `range_expander(x, scaling = 1.5)` returns `c(-5, 15)` (20 units).
#'
#' @section Future Development:
#'   After testing, this function will likely be moved to `jcutils` package.
#'
#' @export
range_expander <- function(..., scaling) {
  actual_range <- range(...)
  extra_range <- ((scaling - 1) / 2) * diff(actual_range)
  new_range <- actual_range + (extra_range * c(-1, 1))

  return(new_range)
}
