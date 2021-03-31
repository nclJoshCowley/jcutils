#' Test Utilities
#'
#' Various function to assist with unit tests.
#'
#' @section Test Data:
#' ## Explanatory Variables
#' The explanatory variables are as follows and prefixed with `expl_`.
#' - **rnorm**, **runif**, etc.
#'   Draws from an arbitrarily chosen distribution of the same name.
#' - **bin**.
#'   Vector made up of TRUE / FALSE values, chosen at random.
#' - **fct**.
#'   Factor column, uppercase letters used as levels.
#' - **fct_drop**.
#'   Factor column, lowercase letters used as levels and one droppable level
#'   with no observations.
#' - **na**.
#'   Column with some values set to NA.
#'
#' ## Response variables:
#' The response variables are a functions of independent linear predictors.
#'   Details are as follows and each variable name is prefixed with `resp_`.
#' - **lp1**, **lp2**. Standard regression, `Y = XB`.
#' - **exp**. Positive values only, `Y = exp(XB)`.
#' - **exp_int**. Positive integers only, `Y = round(exp(XB), 0)`.
#' - **inv_logit**. Inverse logit transformation, `Y = plogis(XB, 0, 1)`.
#' - **na**. Standard regression with missing values mirrored in `expl_na`.
#'
#' @name test_utils
NULL

#' @rdname test_utils
#'
#' @param nrows integer. The number of rows that the output should have.
#' @param incl_na logical. When TRUE, the missing data columns are included.
#'
#' @return test data is a tibble object with all variables contained.
create_test_data <- function(nrows = 20, incl_na = TRUE) {
  # Explanatory variables
  test_expl <- tibble::tibble(
    expl_rnorm = stats::rnorm(nrows, mean = 0, sd = 1),
    expl_runif = stats::runif(nrows, min = 0.5, max = 1.5),
    expl_rbeta = stats::rbeta(nrows, 0.9, 1),
    expl_rgamma = stats::rgamma(nrows, 1, 2),
    expl_rbinom = stats::rbinom(nrows, 10, 0.4),
    expl_rpois = stats::rpois(nrows, 5),
    expl_bin = sample(c(TRUE, FALSE), size = nrows, replace = TRUE),
    expl_fct = factor(
      sample(LETTERS[1:3], size = nrows, replace = TRUE),
      levels = LETTERS[1:3]
    ),
    expl_fct_drop = factor(
      sample(letters[1:4], size = nrows, replace = TRUE),
      levels = letters[1:5]
    )
  )

  # (Observations deleted later)
  if (incl_na) test_expl$expl_na <- stats::rnorm(nrows, mean = 0.5, sd = 2)

  # Create a function that will create coefficients (and thus linear predictors)
  #   such that the coefficients can be accessed later.
  # see ?ff_create_coeffs for more information.
  create_coeffs <- ff_create_coeffs(size = ncol(test_expl))
  create_lp <- function(...) {
    Reduce(
      f = `+`,
      purrr::map2(test_expl, create_coeffs(...), ~ as.numeric(.x) * .y)
    )
  }

  # Response variables
  test_resp <- tibble::tibble(
    resp_lp1 = create_lp(),
    resp_lp2 = create_lp(),
    resp_exp = create_lp(vals = seq(0.01, 0.5, 0.01), pm = FALSE, std_dev = 0.01) %>%
      exp(),
    resp_exp_int = create_lp(vals = seq(0.01, 1, 0.01), pm = FALSE, std_dev = 0.01) %>%
      exp() %>%
      round(),
    resp_inv_logit = create_lp(vals = seq(0.01, 1, 0.01), std_dev = 0.01) %>%
      stats::plogis(location = 0, scale = 1) # (inverse logit)
    # resp_na added later.
  )

  # Missing data correction
  if (incl_na) {
    test_resp$resp_na <- create_lp()
    rm_rows <- sample.int(nrows) %% 5 == 0
    test_expl$expl_na[rm_rows] <- NA_real_
    test_resp$resp_na[rm_rows] <- NA_real_
  }

  # Format data
  testdata <- dplyr::bind_cols(test_resp, test_expl)
  coeffs <- rlang::env_get(environment(create_coeffs), "c_all") %>%
    stats::setNames(colnames(test_resp)) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(expl = colnames(test_expl), .before = 1)
  attr(testdata, "coeffs") <- coeffs

  return(testdata)
}
