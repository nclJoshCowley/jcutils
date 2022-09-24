#' Knitr Defaults
#'
#' Set (custom) default ggplot2 theme and knitr options.
#'
#' @param ... named arguments. Override knitr options.
#' @param base_size numeric. base font size for `ggplot2`, given in pts.
#'
#' @export
knitr_default_setup <- function(..., base_size = 18) {
  args <- rlang::list2(...)

  ggplot2::theme_set(ggplot2::theme_minimal(base_size = base_size))
  ggplot2::theme_update(panel.spacing = ggplot2::unit(2.5, "lines"))

  knitr_opts <-
    list(
      echo = FALSE,
      message = FALSE,
      fig.width = 12,
      fig.asp = 0.55,
      fig.align = "center",
      out.width = "100%"
    )

  knitr::opts_chunk$set(utils::modifyList(knitr_opts, args))
}
