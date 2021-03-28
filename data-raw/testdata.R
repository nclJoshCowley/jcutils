# We consider a list of possible explanatory variable formats of equal length
# Then create some response variable(s) as a linear function of these
# explanatory variables.

# Parameters
set.seed(23)
n_s <- 20 # 150 # 1000


# Explanatory variables
test_expl <- tibble::tibble(
  # Continuous, bell curve about 0
  expl_rnorm = rnorm(n_s, mean = 0, sd = 1),
  # Continuous, uniform about 0
  expl_runif = runif(n_s, min = 0.5, max = 1.5),
  # Binary variable, values are T/F (could be handled different to 2-lvl factor)
  expl_bin = sample(c(TRUE, FALSE), size = n_s, replace = TRUE),
  # Factor variable, 3 levels
  expl_fct = factor(
    sample(LETTERS[1:3], size = n_s, replace = TRUE),
    levels = LETTERS[1:3]
  ),
  # Factor variable, 5 levels with at least one level having 0 occurrences
  expl_drop_fct = factor(
    sample(letters[1:4], size = n_s, replace = TRUE),
    levels = letters[1:5]
  ),
  # Simple explanatory variable with every n^th element = NA
  expl_some_missing = ifelse(
    seq(n_s) %% 5 == 0,
    NA_real_,
    rnorm(n_s, mean = 0.5, sd = 2)
  )
)

# FUN: Creates coefficients and stores them for later. See ?ff_create_coeffs
create_coeffs <- ff_create_coeffs(size = ncol(test_expl))

# FUN: Wrapper around above that gets a vector representing XB = \sum(X_i * b_i)
create_lp <- function(...) {
  Reduce(
    f = `+`,
    purrr::map2(test_expl, create_coeffs(...), ~ as.numeric(.x) * .y)
  )
}

# Response variables
test_resp <- tibble::tibble(
  # Y = XB (1)
  resp_lp1 = create_lp(),
  # Y = XB (2)
  resp_lp2 = create_lp(),
  # Y = exp(XB)  ...  (Y > 0)
  resp_exp = create_lp(vals = seq(0.01, 1.5, 0.01), pm = FALSE, std_dev = 0.01) %>%
    exp(),
  # Y = round(exp(XB))  ...  (Y > 0, Y integer)
  resp_pois = create_lp(vals = seq(0.01, 1, 0.01), pm = FALSE, std_dev = 0.01) %>%
    exp() %>%
    round(),
  # Y = exp(XB) / (1 + exp(XB))  ..  (Y \in [0,1])
  resp_ilogit = create_lp(vals = seq(0.01, 1, 0.01), std_dev = 0.01) %>%
    sapply(function(.x) exp(.x) / (1 + exp(.x)) )
)


# Format data
testdata <- dplyr::bind_cols(test_resp, test_expl)

coeffs <- rlang::env_get(environment(create_coeffs), "c_all") %>%
  setNames(colnames(test_resp)) %>%
  tibble::as_tibble() %>%
  dplyr::mutate(expl = colnames(test_expl), .before = 1)
attr(testdata, "coeffs") <- coeffs

# Inspection by eye
testdata
attributes(testdata)

# Add to package
usethis::use_data(testdata, overwrite = TRUE)
