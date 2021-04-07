#' Representation of p-values
#'
#' Turns numeric values denoting p-values to characters showing a more
#'   informative representation of significance
#'
#' @param pv numeric vector. P-values to be converted.
#' @inheritParams base::round
#'
#' @details Based on widely used:
#'   "Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1"
#'
#' @export
repr_pvals <- function(pv, digits = 3) {
  stopifnot("Some p-values not in (0,1)" = all(pv >= 0 & pv <= 1, na.rm = TRUE))

  pv_rnd <- round(pv, digits)

  dplyr::case_when(
    is.na(pv_rnd) ~ NA_character_,
    pv_rnd < 0.001 ~ "<0.001 (***)",
    pv_rnd <= 0.01 ~ paste0(pv_rnd, " (**)"),
    pv_rnd <= 0.05 ~ paste0(pv_rnd, " (*)"),
    pv_rnd <= 0.10 ~ paste0(pv_rnd, " (.)"),
    TRUE ~ as.character(pv_rnd)
  )
}
