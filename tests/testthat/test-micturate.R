context("micturate")

test_that("micturate-works", {

  example_file <- system.file("extdata/example_norm.csv", package = "micturate")
  example_values <- read.csv(example_file, header = TRUE, stringsAsFactors = FALSE)
  expect_true(is.data.frame(normVols(example_values,100)))
  expect_error(is.data.frame(normVols(example_values,"vol")))
  }
)

