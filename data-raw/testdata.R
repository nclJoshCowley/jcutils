# Set seed to ensure data stays consistent
set.seed(1)

# Add to package
testdata <- jcutils::create_test_data(100)
usethis::use_data(testdata, overwrite = TRUE)
