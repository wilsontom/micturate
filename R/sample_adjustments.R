#' Urine Sample Adjustments
#'
#'
#' @param ri_data a `tbl_df` of **sample_name**, **value**
#' @param norm_value a numeric value to normalise to
#' @param volume a numeric value for the total volume per sample
#'
#' @return a `tbl_df` of pooling volumes for each **sample_name**
#' @export

sampel_adjustments <- function(ri_data, norm_value, volume)
{
  ri_data$value[ri_data$value < norm_value] <- norm_value


  corfac <-
    correction_factor(ri_data$value, norm_value = norm_value)

  norm_vols <- normalisation_volumes(corfac, total_volume = volume)

  ri_data_all <-
    ri_data %>% dplyr::mutate(urine_volume = norm_vols$urine_volume,
                              water_volume = norm_vols$water_volume)


  urine_only <-
    ri_data_all %>% dplyr::select(-value,-water_volume)


  water_only <-
    ri_data_all %>% dplyr::select(-value,-urine_volume)


  adjustments_final <-
    urine_only %>% dplyr::left_join(., water_only, by = 'sample_name')


  return(adjustments_final)


}
