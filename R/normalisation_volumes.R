#' Normalisation Volumes
#'
#' Calculate the required volumes based on normalisation correction factors
#'
#' @param x a numeric vector of correction factors
#' @param total_volume a numeric value specifying the total required volume (urine + water)
#'
#' @return a `tbl_df` of required urine and water volumes
#' @export

normalisation_volumes <- function(x, total_volume = 500)
{

  UrineVol <- total_volume / x
  WaterVol <- total_volume - UrineVol

  volume_tbl <- tibble::tibble(
    correction_factor = x,
    urine_volume = round(UrineVol, digits = 1),
    water_volume = round(WaterVol, digits = 1)
  )

  return(volume_tbl)
}
