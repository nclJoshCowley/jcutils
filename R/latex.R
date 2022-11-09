#' R Object to Latex
#'
#' Generic method that accepts some R object and converts it to LaTeX.
#'
#' @param x object used to choose a method.
#' @param ... extra arguments passed to method.
#'
#' @export
to_latex <- function(x, ...) {
  UseMethod("to_latex")
}

#' @rdname to_latex
#'
#' @param digits integer. Passed to [format()].
#' @param envir character. LaTeX matrix style.
#'
#' @export
to_latex.matrix <- function(x, digits = NULL, envir = "pmatrix", ...) {

  x <- format(x, digits = digits)
  rstrings <- apply(x, 1, paste, collapse = " & ")

  body <- paste0(rstrings, collapse = " \\\\ \n    ")
  body <- paste0("    ", body)

  head <- paste0("\\begin{", envir, "}")
  foot <- paste0("\\end{", envir, "}")

  return(paste0(c(head, body, foot), collapse = "\n"))
}


#' @rdname to_latex
#' @export
to_latex.numeric <- function(x, digits = NULL, envir = "pmatrix", ...) {
  dim(x) <- c(length(x), 1)
  to_latex.matrix(x, digits = digits, envir = envir)
}
