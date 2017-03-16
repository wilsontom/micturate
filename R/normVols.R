#' Create normalisation volumes
#'
#' @param df a \code{data.frame} with the following columns
#' \describe{
#' \item{id}
#' \item{value}
#' \item{cls}
#' }
#' @return a \code{data.frame} with the following columns
#' \item{id}
#' \item{value}
#' \item{cls}
#' \item{UrineVol}
#' \item{WaterVol}
#'
#' @author Tom Wilson \email{tpw2@@aber.ac.uk}
#' @export

normVols <- function(df, volume)
  {
  if(!"cls" %in% names(df)){stop(deparse(substitute(df)), " doesnt contain a 'cls' column", call. = FALSE)}
  if(!"value" %in% names(df)){stop(deparse(substitute(df)), " doesnt contain a 'value' column", call. = FALSE)}

  if(!is.numeric(volume)){stop(deparse(substitute(volume)), " must be a numeric value", call. = FALSE)}


  dfsp <- split(df, f = df[,"cls"])
  dfcor <- lapply(dfsp, function(x)(corfac(x$value, volume)))

  dfres <- NULL
  for(i in seq_along(dfsp)){
    dfres[[i]] <- data.frame(dfsp[[i]], dfcor[[i]])
  }

  dfinal <- do.call("rbind", dfres)
  dfinal$UrineVol <- round(dfinal$UrineVol, digits = 2)
  dfinal$WaterVol <- round(dfinal$WaterVol, digits = 2)

  return(dfinal)
  }


