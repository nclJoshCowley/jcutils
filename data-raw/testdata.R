# Set seed to ensure data stays consistent
set.seed(1)

# Create data
testdata <- jcutils::create_testdata(nrows = 100)
testdata

# Add to package
usethis::use_data(testdata, overwrite = TRUE)
