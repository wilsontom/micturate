context("micturate")

test_that("micturate-works", {
  example_file <-
    system.file("extdata/example_norm.csv", package = "micturate")
  example_values <- readr::read_csv(example_file)

  example_gaml <-
    system.file("extdata/example_ri_gaml.gaml", package = "micturate")


  example_refs <- system.file("extdata/external_nacl_ri.csv", package = "micturate")
  example_refs <- readr::read_csv(example_refs)


  expect_true(tibble::is_tibble(refractive_index_prediction(example_gaml, standards = NULL)))


  expect_error(refractive_index_prediction(example_gaml, standards = example_values))

  expect_true(tibble::is_tibble(refractive_index_prediction(example_gaml, standards = example_refs)))






})
