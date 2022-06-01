#' Predict RI values from HPLC-RI
#'
#'
#' @param gaml_file a GAML file containing HPLC-RI data. See micturate::plate_maps('urine_ri') for more details
#' @return a `tbl_df` of predicted refractive index values
#' @export
#' @importFrom magrittr %>%
#' @importFrom stats lm predict

refractive_index_prediction <- function(gaml_file)
{

  gaml_tbl <- gaml2r::openFile(gaml_file, output = 'tbl_df')

  max_ri_values <- gaml_tbl %>%
    dplyr::group_by(sample_name) %>%
    dplyr::summarise(value = max(value))

  standards_only <- nacl_sg_standards %>%
    dplyr::left_join(., max_ri_values, by = 'sample_name')

  linear_train_model <-
    lm(specific_gravity ~ value, data = standards_only)


  predict_ri <-
    predict(linear_train_model, newdata = data.frame(value = max_ri_values$value)) %>% round(., digits = 4)


  ri_results <- max_ri_values %>% dplyr::mutate(predicted_value = predict_ri)


  return(ri_results)

}


