context("micturate")

test_that("micturate-works", {

  example_values <- read.csv(system.file("extdata/example_norm.csv", package = "micturate"), head = TRUE,
                                stringsAsFactors = FALSE)

   expect_true(is.data.frame(normVols(example_values,100)))
   expect_error(is.data.frame(normVols(example_values,"vol")))
  }
)

