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

refractive_index_prediction <- function(gaml_file, standards)
{
  gaml_tbl <- gaml2r::openFile(gaml_file, output = 'tbl_df', include_injection_info = TRUE) %>%
    dplyr::select(sample_name, value, position)

  max_ri_values <- gaml_tbl %>%
    dplyr::group_by(sample_name) %>%
    dplyr::summarise(value = max(value))



  if (!is.null(standards)) {
    standards_only <- standards %>%
      dplyr::left_join(., max_ri_values, by = 'sample_name')
  } else{
    data(nacl_sg_standards)
    standards_only <- nacl_sg_standards %>%
      dplyr::left_join(., max_ri_values, by = 'sample_name')
  }


  linear_train_model <-
    lm(specific_gravity ~ value, data = standards_only)


  predict_ri <-
    predict(linear_train_model, newdata = data.frame(value = max_ri_values$value)) %>% round(., digits = 4)


  ri_results <-
    max_ri_values %>% dplyr::mutate(predicted_value = predict_ri)


  sample_position <-
    gaml_tbl %>% dplyr::select(-value) %>% dplyr::distinct()

  ri_final <- ri_results %>% dplyr::left_join(., sample_position, by = 'sample_name')



  return(ri_results)

}
