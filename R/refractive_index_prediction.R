#' Predict RI values from HPLC-RI
#'
#'
#' @param gaml_file a GAML file containing HPLC-RI data.
#' @param standards a two column `tbl_df` of external calibration standards. Columns must be named `sample_name` and `specific_gravity`. `sample_name` should use the format `S01`, `S02`, `S03`, etc...
#' If `NULL` then a set of package calibration standards will be used instead.
#' @return a `tbl_df` of predicted refractive index values
#' @export
#' @importFrom magrittr %>%
#' @importFrom stats lm predict

decode_gaml_array <- function(x, size = 8)
{
  encoded_values <- gsub("\\s+", "", x)
  raw_values <- base64enc::base64decode(encoded_values)

  readBin(
    raw_values,
    what = "double",
    n = length(raw_values) / size,
    size = size,
    endian = .Platform$endian
  )
}

extract_gaml_max_ri <- function(experiment)
{
  sample_name <- xml2::xml_attr(experiment, "name")

  position <- xml2::xml_find_first(experiment, "./parameter[@name='position']") %>%
    xml2::xml_text()

  y_values <- xml2::xml_find_first(experiment, ".//Ydata/values") %>%
    xml2::xml_text() %>%
    decode_gaml_array()

  tibble::tibble(
    sample_name = sample_name,
    value = max(y_values),
    position = position
  )
}

read_gaml_max_ri <- function(gaml_file)
{
  if (tools::file_ext(gaml_file) != "gaml") {
    stop("`gaml_file` must be a .gaml file.", call. = FALSE)
  }

  xml_doc <- xml2::read_xml(gaml_file)
  experiments <- xml2::xml_find_all(xml_doc, ".//experiment")

  dplyr::bind_rows(lapply(experiments, extract_gaml_max_ri))
}

refractive_index_prediction <- function(gaml_file, standards)
{
  max_ri_values <- read_gaml_max_ri(gaml_file)

  if (!is.null(standards)) {
    standards_only <- standards %>%
      dplyr::left_join(., max_ri_values, by = 'sample_name')
  } else{
    standards_only <- nacl_sg_standards %>%
      dplyr::left_join(., max_ri_values, by = 'sample_name')
    }


  linear_train_model <-
    lm(specific_gravity ~ value, data = standards_only)


  predict_ri <-
    predict(linear_train_model, newdata = data.frame(value = max_ri_values$value)) %>% round(., digits = 4)


  ri_results <-
    max_ri_values %>% dplyr::mutate(predicted_value = predict_ri) %>%
    dplyr::select(sample_name, value, predicted_value, position)



  return(ri_results)

}
