#' Representation of p-values
#' Deprecated in favour of \code{\link{format_p_values}}.
#' @export
repr_pvals <- function(pv, digits = 3) {
  .Deprecated(
    new = "format_p_values",
    package = "jcutils",
    msg = paste(
      "jcutils::repr_pvals() deprecated.",
      "Please use jcutils::format_p_values()."
    )
  )
}
