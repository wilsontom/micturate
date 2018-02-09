#' Create normalisation correction factors
#' @keywords internal

corfac <- function(x, volume)
{
  xmin <- x[which(x == min(x))]
  if (length(xmin) > 1) {
    xmin <- xmin[1]
  }
  xcor <- (x - 1) / (xmin - 1)
  reqVolU <- (volume / xcor)

  reqVolW <- (volume - reqVolU)

  voldf <- data.frame(UrineVol = reqVolU, WaterVol = reqVolW)

  return(voldf)

}
