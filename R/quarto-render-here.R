#' Quarto Render
#'
#' Copies the chosen quarto file and YAML and renders within that directory.
#'
#' @param input path. Quarto input file.
#' @param output_dir directory. Target directory to render target in.
#' @param with_yaml logical. Should all `.yml` files also be used.
#' @param ... passed to [quarto::quarto_render()].
#'
#' @export
quarto_render_here <- function(input, output_dir, ..., with_yaml = TRUE) {
  if (!dir.exists(output_dir)) stop("Couldn't find directory: ", output_dir)

  in_dir <- dirname(input)
  in_yml <- list.files(in_dir, "\\.yml", full.names = TRUE)

  file.copy(input, output_dir, overwrite = TRUE)
  if (with_yaml) file.copy(in_yml, output_dir, overwrite = TRUE)

  quarto::quarto_render(input = file.path(output_dir, basename(input)), ...)

  debug <- rlang::list2(...)$debug
  if (isFALSE(debug) || is.null(debug)) {
    unlink(file.path(output_dir, basename(input)))
    if (with_yaml) unlink(file.path(output_dir, basename(in_yml)))
  }

  invisible(NULL)
}
