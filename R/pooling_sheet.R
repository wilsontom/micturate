#' Urine Pooling Sheet
#'
#'
#' @param ri_data a `tbl_df` of **sample_name**, **value** and **group**
#' @param norm_value a numeric value to normalise to
#' @param volume a numeric value for the total volume per sample
#'
#' @return a `tbl_df` of pooling volumes for each **sample_name**
#' @export

pooling_sheet <- function(ri_data, norm_value, volume)
{

  corfac <- correction_factor(ri_data$value, norm_value = norm_value)

  norm_vols <- normalisation_volumes(corfac, total_volume = volume)

  ri_data_all <-
    ri_data %>% dplyr::mutate(urine_volume = norm_vols$urine_volume,
                              water_volume = norm_vols$water_volume)


  urine_only <-
    ri_data_all %>% dplyr::select(-value,-water_volume) %>%
    tidyr::pivot_wider(names_from = group, values_from = urine_volume)

  water_only <-
    ri_data_all %>% dplyr::select(-value,-urine_volume) %>%
    dplyr::group_by(sample_name) %>% dplyr::summarise(water = sum(water_volume))


  pooling_final <- urine_only %>% dplyr::left_join(., water_only, by = 'sample_id')


  return(pooling_final)


}
