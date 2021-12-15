# -------------------------------------------------------------------------|
# Utilities that extend the usefulness of functions found in base.
# -------------------------------------------------------------------------|


#' Invert Symmetric Positive Definite Matrix via Cholesky Decomposition
#'
#' Consider a matrix that is symmetric and has positive eigenvalues, and is
#'   therefore positive definite. Then finds the inverse of the cholesky
#'   decomposition of this matrix as a more numerically stable alternative
#'   to `base::solve(x)`.
#'
#' @param x matrix. symmetric (up to `tol`) and positive definite.
#' @param tol numeric. Differences of symmetry smaller than this value
#'   are not considered significant.
#'
#' @return the matrix inverse of `x`.
#'
#' @export
chol_solve <- function(x, tol = 100 * .Machine$double.eps) {
  stopifnot(
    "x is not symmetric" = isSymmetric(x, tol),
    "x is not positive definite" = all(eigen(x)$values > tol)
  )
  return(chol2inv(chol(x)))
}


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
