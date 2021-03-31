# Load in consistent testdata
data("testdata", package = "jcutils")
resp <- dplyr::select(testdata, dplyr::starts_with("resp"))
expl <- dplyr::select(testdata, dplyr::starts_with("expl"))

# Put models into same list to be saved
testmodels <- list()

# Linear Model, lm()
testmodels$fit_lm <- stats::lm(resp$resp_lp1 ~ ., data = expl)

# Multiple Linear Model, mlm()
testmodels$fit_mlm <- stats::lm(
  cbind(resp$resp_lp1, resp$resp_lp2) ~ .,
  data = expl
)

# Binomial regression
testmodels$fit_glm_binomial <- stats::glm(
  resp$resp_binom ~ .,
  data = expl,
  family = "binomial"
)

# Poisson regression
testmodels$fit_glm_poisson <- stats::glm(
  resp$resp_exp_int ~ .,
  data = expl,
  family = "poisson"
)

# More to be added ...

# Add to package
testmodels
usethis::use_data(testmodels, overwrite = TRUE)
