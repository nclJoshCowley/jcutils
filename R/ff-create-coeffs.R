#' Create Coefficients (Function Factory)
#'
#' Will return a function that is capable of returning different coefficients
#'   each time while storing the generated coefficients in an object stored
#'   in the function environment.
#'
#' @details For more details on function factories see
#'   [Advanced R: Function Factories.](https://adv-r.hadley.nz/function-factories.html)
#'
#' @param size numeric. Number of coefficients in each vector.
#' @param vals numeric. Possible absolute values for the coefficients to take.
#' @param pm (plus / minus) logical. When TRUE coefficients can be
#'   initially set to \[-vals, vals\].
#' @param std_dev numeric. The SD of the nugget term added to each coefficient.
#'
#' @return A call such as `x <- ff_create_coeffs(...)` would allow `x` to be a
#'   callable object that returns coefficients that can later be accessed using
#'   `rlang::env_get(environment(x), "c_all")`.
ff_create_coeffs <- function(size, vals = seq(20), pm = TRUE, std_dev = 0.1) {
  force(size)
  vals_default <- vals
  pm_default <- pm
  std_dev_default <- std_dev

  c_all <- list()

  function(vals = vals_default, pm = pm_default, std_dev = std_dev_default) {
    # Absolute values?
    c_val <- sample(vals, size)
    # Sign of each coefficient? (+1 or -1)
    c_sign <- if (pm) sample(c(-1, 1), size, replace = TRUE) else 1
    # Store and return
    c_i <- (c_val * c_sign) + stats::rnorm(size, sd = std_dev)
    c_all <<- c(c_all, list(c_i))
    return(c_i)
  }
}
