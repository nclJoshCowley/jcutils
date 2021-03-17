# We consider a list of possible explanatory variable formats of equal length
# Then create some response variable(s) as a linear function of these
# explanatory variables.

# Consistent output
set.seed(23)

n_s <- 20 # 150 # 1000

testdata <- tibble::tibble(
  # Continuous, bell curve about 0
  expl_rnorm = rnorm(n_s, mean = 0, sd = 1),
  # Continuous, uniform about 0
  expl_runif = runif(n_s, min = 0, max = 1),
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
  )
)

# Only 1 response variable for now
coeff <- sample(seq(-9, 9), size = ncol(testdata))
attr(testdata, "coeff") <- coeff

testdata <- tibble::add_column(
  testdata,
  resp = Reduce(`+`, lapply(seq_along(testdata), function(j) {
    coeff[j] * as.numeric(testdata[[j]])
  })),
  .before = 1
)

# Add to package
testdata
attributes(testdata)
usethis::use_data(testdata, overwrite = TRUE)
