
#' @export
#' @param volume a numeric value for the total volume of each single sample to be used
#' - __`id`__ Unique sample identifier
#' - __`value`__ A value for urine conenctration (refractive index, osmolarity, etc..)
#' - __`cls`__ The class which sample will be grouped into for pooling



plot_refractive_index <- function(df)
  {

  upper <- mean(df[,'value']) + (2 * sd(df[,'value']))
  lower <- mean(df[,'value']) - (2 * sd(df[,'value']))
  centre <- mean(df[,'value'])

  numseq <- seq(from = 1, to = nrow(df), by = 1)
  df[,'id'] <- numseq

  scatter_plot <-
    ggplot(df, aes(y = value, x = id, colour = factor(cls))) + geom_point(size = 2) + theme_bw() +
    geom_hline(yintercept = centre, colour = 'blue') +
    geom_hline(yintercept = upper, colour = 'red') +
    geom_hline(yintercept = lower, colour = 'red') +
    theme(
      axis.text.y = element_text(size = 11, face = 'bold'),
      axis.text.x = element_text(size = 11, face = 'bold'),
      axis.title.y = element_text(size = 11, face  = 'bold'),
      axis.title.x = element_text(size = 11, face = 'bold')
    ) + labs(
      x = '',
      y = 'Refractive Index',
      title = 'Distribution of Refractive Index (RI) Values',
      subtitle = 'Blue line is mean(RI), Red Lines are mean(RI) +/- 2 * sd(RI)'
    )


  density_plot <-
    ggplot(df, aes(x = value)) + geom_density(fill = 'red', alpha = 0.3) + theme_bw() +
    theme(
      axis.text.y = element_text(size = 11, face = 'bold'),
      axis.text.x = element_text(size = 11, face = 'bold'),
      axis.title.y = element_text(size = 11, face  = 'bold'),
      axis.title.x = element_text(size = 11, face = 'bold')
    ) + labs(x = 'Refractive Index Value', y = 'Density')


   print(scatter_plot + density_plot)

   return(invisible(NULL))
}


