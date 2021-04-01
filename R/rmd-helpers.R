#' Nice Table in R Markdown.
#'
#' This function utilises the flexibility of kableExtra to choose some 'nice'
#'   default tables for either HTML or PDF via LaTeX. (subjective)
#'
#' @param x data.frame. Data to be converted into a table.
#' @param fmt choice. Is the table to be displayed via HTML ("html")
#'   or via PDF ("latex").
#' @param scroll_h numeric. If the format is HTML, create a scroll box of this
#'   height (in pixels)
#' @param ... extra arguments to be passed to either `kableExtra::kbl`
#'   or `kableExtra::kable_styling`
#'
#' @section Future Development:
#' - Add in some method to alter column widths, likely as a ncol-length simplex.
#'
#' @return Either a HTML table or LaTeX table, depending on format.
#'
#' @export
rmd_table <- function(x, fmt = c("html", "latex"), scroll_h = NULL, ...) {
  # Parse arguments
  fmt <- match.arg(fmt)
  dots <- list(...)

  # Validation
  stopifnot("'x' should inherit from data.frame" = inherits(x, "data.frame"))
  if (!is.null(scroll_h)) {
    if (fmt == "latex") stop("Scroll box in HTML only.")
    if (!is.numeric(scroll_h)) stop("Scroll box argument should be numeric.")
  }

  # Obtain (modified) default arguments
  kbl_args <- formals(kableExtra::kbl)
  kbl_args$x <- quote(x)
  kbl_args$format <- fmt
  kbl_args$digits <- 3
  kbl_args$row.names <-  FALSE
  kbl_args$align <- "c"
  kbl_args$... <- NULL
  if (fmt == "latex") {
    kbl_args$booktabs <- TRUE
    kbl_args$linesep <- ""
    kbl_args$table.envir <- "table"
  }

  kbl_args_with_dots <- c(
    kbl_args[!names(kbl_args) %in% names(dots)],
    dots[names(dots) %in% names(kbl_args)]
  )

  # Obtain (modified) default style arguments
  kbl_styles <- formals(kableExtra::kable_styling)
  kbl_styles$bootstrap_options <- c("striped", "hover", "responsive")
  if (fmt == "latex") kbl_styles$latex_options <- c("striped", "HOLD_position")
  kbl_styles$full_width <- TRUE
  if (fmt == "html") kbl_styles$fixed_thead <- TRUE

  kbl_styles_with_dots <- c(
    kbl_styles[!names(kbl_styles) %in% names(dots)],
    dots[names(dots) %in% names(kbl_styles)]
  )

  # Call to kbl() with relevant arguments
  call_kbl <- as.call(c(
    list(quote(kableExtra::kbl)),
    kbl_args_with_dots
  ))
  kbl_styles_with_dots$kable_input <- call_kbl

  # Call to kable_styling() with relevant arguments
  call_kblstyle <- as.call(c(
    list(quote(kableExtra::kable_styling)),
    kbl_styles_with_dots
  ))


  # Return with option scroll box (vertical only)
  out <- eval(call_kblstyle, rlang::env(booktabs = TRUE))
  if (!is.null(scroll_h)) {
    kableExtra::scroll_box(out, width = "100%", height = paste0(scroll_h, "px"))
  } else {
    out
  }
}


#' Nice Model Summary in R Markdown
#'
#' A better looking summary table with reference columns, rounding, p-value
#'   highlighting that can be called directly in RMD.
#'
#' @param object Typical fit object such as "lm", "mlm", "glm", etc.
#' @inheritParams rmd_table
#' @param digits numeric. Significant figures to round to for all numerics
#'   other than p values.
#' @param resp regular expression. Should match a single response variable.
#'   Will be silently ignored for non-mlm objects.
#'
#' @export
rmd_summary <- function(object, fmt = c("html", "latex"), digits = 3, resp) {
  UseMethod("rmd_summary", object)
}

#' @rdname rmd_summary
#' @export
rmd_summary.default <- function(object, fmt = c("html", "latex"), digits = 3, resp) {
  # Want reference levels, do this by looking at original model frame
  coeff_nms <- tibble::as_tibble(stats::model.frame(object)) %>%
    dplyr::select(-1) %>%
    dplyr::summarise(dplyr::across(
      tidyselect::everything(),
      function(.x) dplyr::case_when(
        is.logical(.x) ~ list(c("FALSE", "TRUE")),
        is.factor(.x) ~ list(levels(.x)),
        TRUE ~ list("")
      )
    )) %>%
    tidyr::pivot_longer(
      tidyselect::everything(),
      names_to = "name1",
      values_to = "name2"
    ) %>%
    tidyr::unnest(.data$name2) %>%
    tibble::add_row(name1 = "(Intercept)", name2 = "", .before = 1) %>%
    dplyr::mutate(term = paste0(.data$name1, .data$name2))

  # Add in information provided by broom::tidy()
  coeff_tb <- dplyr::full_join(
    coeff_nms,
    broom::tidy(object, conf.int = TRUE),
    by = "term"
  )

  # Temporary function for formatting
  reformat <- function(x, r = "") ifelse(is.na(x), r, format(x, digits = digits))

  # Return as call to rmd_table
  coeff_tb %>%
    dplyr::transmute(
      Coefficients = ifelse(
        nchar(.data$name2) > 0,
        paste0(.data$name1, " (", .data$name2, ")"),
        .data$term
      ),
      Estimate = reformat(.data$estimate, "Ref"),
      `CI (95%)` = ifelse(
        is.na(.data$conf.low),
        "",
        paste0("(", reformat(.data$conf.low), ", ", reformat(.data$conf.high), ")")
      ),
      `Std Error` = reformat(.data$std.error, ""),
      Statistic = reformat(.data$statistic, ""),
      P = ifelse(is.na(.data$p.value), "", jcutils::repr_pvals(.data$p.value))
    ) %>%
    jcutils::rmd_table(fmt)
}

#' @rdname rmd_summary
#' @export
rmd_summary.mlm <- function(object, fmt = c("html", "latex"), digits = 3, resp) {
  # Want reference levels, do this by looking at original model frame
  coeff_nms <- tibble::as_tibble(stats::model.frame(object)) %>%
    dplyr::select(-1) %>%
    dplyr::summarise(dplyr::across(
      tidyselect::everything(),
      function(.x) dplyr::case_when(
        is.logical(.x) ~ list(c("FALSE", "TRUE")),
        is.factor(.x) ~ list(levels(.x)),
        TRUE ~ list("")
      )
    )) %>%
    tidyr::pivot_longer(
      tidyselect::everything(),
      names_to = "name1",
      values_to = "name2"
    ) %>%
    tidyr::unnest(.data$name2) %>%
    tibble::add_row(name1 = "(Intercept)", name2 = "", .before = 1) %>%
    dplyr::mutate(term = paste0(.data$name1, .data$name2))

  # Validate response name
  if (missing(resp)) {
    stop("Need 'resp' argument for mlm")
  } else {
    resp_nm <- grep(resp, as.character(object$terms[[2]]), value = TRUE)
    if (length(resp_nm) < 1) stop("Couldn't match 'resp' to call")
    if (length(resp_nm) > 1) stop("Multiple matches of 'resp' in call")
  }

  # Add in information provided by broom::tidy()
  coeff_tb <- dplyr::full_join(
    coeff_nms,
    broom::tidy(object, conf.int = TRUE) %>%
      dplyr::filter(.data$response == resp_nm) %>%
      dplyr::select(-.data$response),
    by = "term"
  )

  # Temporary function for formatting
  reformat <- function(x, r = "") ifelse(is.na(x), r, format(x, digits = digits))

  # Return as call to rmd_table
  coeff_tb %>%
    dplyr::transmute(
      Coefficients = ifelse(
        nchar(.data$name2) > 0,
        paste0(.data$name1, " (", .data$name2, ")"),
        .data$term
      ),
      Estimate = reformat(.data$estimate, "Ref"),
      `CI (95%)` = ifelse(
        is.na(.data$conf.low),
        "",
        paste0("(", reformat(.data$conf.low), ", ", reformat(.data$conf.high), ")")
      ),
      `Std Error` = reformat(.data$std.error, ""),
      Statistic = reformat(.data$statistic, ""),
      P = ifelse(is.na(.data$p.value), "", jcutils::repr_pvals(.data$p.value))
    ) %>%
    jcutils::rmd_table(fmt)
}
