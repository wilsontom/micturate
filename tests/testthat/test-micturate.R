test_that("correction_factor returns expected values", {
  expect_equal(
    correction_factor(c(1.006, 1.009)),
    c(1, 1.5)
  )

  expect_equal(
    correction_factor(1.004, norm_value = 1.004),
    1
  )
})

test_that("normalisation_volumes returns rounded urine and water volumes", {
  expect_equal(
    normalisation_volumes(c(1, 2), total_volume = 500),
    tibble::tibble(
      correction_factor = c(1, 2),
      urine_volume = c(500, 250),
      water_volume = c(0, 250)
    )
  )
})

test_that("sampel_adjustments clamps low values before calculating volumes", {
  ri_data <- tibble::tibble(
    sample_name = c("ID01", "ID02"),
    value = c(1.008, 1.004)
  )

  expect_equal(
    sampel_adjustments(ri_data, norm_value = 1.006, volume = 600),
    tibble::tibble(
      sample_name = c("ID01", "ID02"),
      urine_volume = c(450, 600),
      water_volume = c(150, 0)
    )
  )
})

test_that("pooling_sheet widens group volumes and sums water volumes", {
  ri_data <- tibble::tibble(
    sample_name = c("ID01", "ID01", "ID02", "ID02"),
    value = c(1.008, 1.005, 1.012, 1.006),
    group = c("A", "B", "A", "B")
  )

  expect_equal(
    pooling_sheet(ri_data, norm_value = 1.006, volume = 600),
    tibble::tibble(
      sample_name = c("ID01", "ID02"),
      A = c(450, 300),
      B = c(600, 600),
      water = c(150, 300)
    )
  )
})

test_that("refractive_index_prediction validates the input file extension", {
  example_csv <- system.file("extdata/external_nacl_ri.csv", package = "micturate")

  expect_error(
    refractive_index_prediction(example_csv, standards = NULL),
    "must be a \\.gaml file"
  )
})

test_that("refractive_index_prediction parses example GAML data", {
  example_file <- system.file("extdata/example_norm.csv", package = "micturate")
  example_values <- readr::read_csv(example_file, show_col_types = FALSE)

  example_gaml <- system.file("extdata/example_ri_gaml.gaml", package = "micturate")

  example_refs <- system.file("extdata/external_nacl_ri.csv", package = "micturate")
  example_refs <- readr::read_csv(example_refs, show_col_types = FALSE)

  default_predictions <- refractive_index_prediction(example_gaml, standards = NULL)
  external_predictions <- refractive_index_prediction(example_gaml, standards = example_refs)

  expect_s3_class(default_predictions, "tbl_df")
  expect_s3_class(external_predictions, "tbl_df")
  expect_named(
    default_predictions,
    c("sample_name", "value", "predicted_value", "position")
  )
  expect_true(all(!is.na(default_predictions$predicted_value)))
  expect_equal(nrow(default_predictions), nrow(external_predictions))

  expect_error(
    refractive_index_prediction(example_gaml, standards = example_values)
  )
})
