#' Create normalisation volumes
#'
#' Calculate the required Urine:Water volume for creating pools from multiple single urine samples, based
#' on a measure of urine concentration (ie, refractive index)
#'
#' @param df a `data.frame` with the following columns
#' @param volume a numeric value for the total volume of each single sample to be used
#' - __`id`__ Unique sample identifier
#' - __`value`__ A value for urine conenctration (refractive index, osmolarity, etc..)
#' - __`cls`__ The class which sample will be grouped into for pooling
#'
#' @return a `data.frame` with the following columns
#' - __`id`__ Unique sample identifier
#' - __`value`__ A value for urine conenctration (refractive index, osmolarity, etc..)
#' - __`cls`__ The class which sample will be grouped into for pooling
#' - __`UrineVol`__ The volume of urine required
#' - __`WaterVol`__  The volume of water required
#'
#' @author Tom Wilson `tpw2@aber.ac.uk`
#' @export

normVols <- function(df, volume)
{
  if (!"cls" %in% names(df)) {
    stop(deparse(substitute(df)),
         " doesnt contain a 'cls' column",
         call. = FALSE)
  }
  if (!"value" %in% names(df)) {
    stop(deparse(substitute(df)),
         " doesnt contain a 'value' column",
         call. = FALSE)
  }

  if (!is.numeric(volume)) {
    stop(deparse(substitute(volume)), " must be a numeric value", call. = FALSE)
  }

  dfsp <- split(df, f = df[, "cls"])
  dfcor <- lapply(dfsp, function(x)
    (corfac(x$value, volume)))

  dfres <- NULL
  for (i in seq_along(dfsp)) {
    dfres[[i]] <- data.frame(dfsp[[i]], dfcor[[i]])
  }

  dfinal <- do.call("rbind", dfres)
  dfinal$UrineVol <- round(dfinal$UrineVol, digits = 2)
  dfinal$WaterVol <- round(dfinal$WaterVol, digits = 2)

  return(dfinal)
}
