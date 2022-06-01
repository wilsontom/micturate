#' Correction Factor
#'
#' Calculate the normalisation correction factor
#'
#' @param x a numeric vector of refractive index values
#' @param norm_value a numeric value specifying the target value to normalise to
#'
#' @return a numeric vector of correction factors
#' @export

correction_factor <- function(x, norm_value = 1.006)
{

  xcor <- (x - 1) / (norm_value - 1)

  return(xcor)
}
