#' Assign iterations mcarray
#'
#' Assign values to an mcarray object. Note this is not straightforward because
#'   for a scalar one would use `x[, 1:10, ] <- value`, whereas for a matrix
#'   one would use `x[, , 1:10, ] <- value`.
#'
#' @param x mcarray object.
#' @param value mcarray object with same dimension except for the penultimate
#'   dimension (iterations).
assign_iterations_mcarray <- function(x, value, at) {
  dx <- dim(x)
  dv <- dim(value)

  nx_iter <- dx[length(dx) - 1]
  nx_chains <- dx[length(dx)]
  nx_vars <- prod(utils::head(dx, -2))

  nv_iter <- dv[length(dv) - 1]
  nv_chains <- dv[length(dv)]
  nv_vars <- prod(utils::head(dv, -2))

  stopifnot(
    "No. of chains incompatible" =
      all.equal(nx_chains, nv_chains, check.names = FALSE),

    "Variable dims incompatible" =
      all.equal(nx_vars, nv_vars, check.names = FALSE),

    "Replacement length not equal to 'at'" =
      all.equal(length(at), nv_iter, check.names = FALSE)
  )

  x_flat <- structure(x, dim = c(nx_vars, nx_iter, nx_chains))
  value_flat <- structure(value, dim = c(nv_vars, nv_iter, nv_chains))

  x_flat[, at, ] <- value_flat

  return(structure(x_flat, dim = dx))
}


# Multidimensional test ---
x <- array(seq(180), dim = c(2, 2, 15, 3))
value <- 1000 * x[, , 6:10, ]
at <- 6:10

assign_iterations_mcarray(x, value, at)


# Single dimensional test ---
x <- array(seq(180), dim = c(4, 15, 3))
value <- 1000 * x[, 6:10, ]
at <- 6:10

assign_iterations_mcarray(x, value, at)


# Scalar test ----
x <- array(seq(45), dim = c(1, 15, 3))
value <- 1000 * x[, 6:10, ]
at <- 6:10

assign_iterations_mcarray(x, value, at)
