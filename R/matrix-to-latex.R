#' Convert Matrix Object to LaTeX Code
#'
#' Will take a matrix (numeric or character) and produce code that can be
#'   placed into a .TeX document.
#'
#' @param x object to be converted.
#' @param envir character. The environment the matrix will be surrounded by.
#'   Defaults to `\\begin{pmatrix}  \\end{pmatrix}`.
#'
#' @return a character is returned that produces the intended formatting when
#'   it is passed to `cat()` or `writeLines()`.
#'
#' @export
matrix_to_latex <- function(x, envir = "pmatrix") {
  x_by_row <- apply(x, 1, paste, collapse = " & ")

  x_tex <- paste0(
    "\\begin{", envir, "}\n  ",
    paste0(x_by_row, collapse = " \\\\ \n  "),
    "\n\\end{", envir, "}"
  )

  x_tex
}
