# -------------------------------------------------------------------------|
# Utilities that provide interface between R and LaTeX.
# -------------------------------------------------------------------------|

#' Convert Matrix Object to LaTeX
#'
#' Convert matrix to a (LaTeX) string to be printed "asis".
#'
#' @param x object to be converted.
#' @param digits numeric. Rounding value for better value display.
#' @param envir character. LaTeX matrix style,
#'   see <https://www.overleaf.com/learn/latex/Matrices>.
#'
#' @return character that produces the intended formatting when passed
#'   to \code{\link[base]{cat}} or \code{\link[knitr]{asis_output}}.
#'
#' @export
matrix_to_latex <- function(x, digits, envir = "pmatrix") {
  x <- format(x, digits = digits)
  rstrings <- apply(x, 1, paste, collapse = " & ")

  body <- paste0(rstrings, collapse = " \\\\ \n    ")
  body <- paste0("    ", body)

  head <- paste0("\\begin{", envir, "}")
  foot <- paste0("\\end{", envir, "}")

  paste0(c(head, body, foot), collapse = "\n")
}
