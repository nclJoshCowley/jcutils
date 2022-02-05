#' Package Data with Calls
#'
#' Similar to base \code{\link[utils]{data}} but also returns calls that can be
#'   assigned to a variable (via \code{eval}) instead of attached.
#'
#' @param package character. Name of the package.
package_data_with_calls <- function(package) {
  info <- utils::data(package = package)$results
  item <- info[, "Item"]
  data_title <- info[, "Title"]

  cl <-
    purrr::map(
      rlang::parse_exprs(item),
      ~ rlang::call2("::", rlang::parse_expr(package), .x)
    )

  tibble::tibble(title = data_title, item = item, call = cl)
}

package_data_with_calls("dplyr")
