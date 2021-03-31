#' Invert Symmetric Positive Definite Matrix via Cholesky Decomposition
#'
#' Consider a matrix that is symmetric and has positive eigenvalues, and is
#'   therefore postive definite. Then finds the inverse of the cholesky
#'   decomposition of this matrix as a more numerically stable alternative
#'   to `base::solve(x)`.
#'
#' @param x matrix. symmetric (up to `tol`) and positive definite.
#' @inheritParams base::isSymmetric
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
