# Title     : Graphs VUI.21.OCT.01
# Objective : Graphs for Variant Under Investigation VUI.21.OCT.01
# Created by: 36112985
# Created on: 27/11/2021

#' @param vui21oct01:
ProgressionVUI21OCT01 <- function (vui21oct01) {

  ggplot(data = vui21oct01, mapping = aes(x = week, y = `VUI.21OCT.01`)) +
    geom_line(alpha = 0.35) +
    geom_point(alpha = 0.35) +
    theme_minimal()  +
    theme(axis.text.x = element_text(size = 11, angle = 90),
          axis.text.y = element_text(size = 11),
          axis.title.x = element_text(face = 'bold', size = 13),
          axis.title.y = element_text(face = 'bold', size = 13),
          legend.title = element_text(),
          panel.grid.minor = element_blank(), panel.grid.major = element_line(size = 0.15)) +
    xlab('\n') +
    ylab('variant fraction\n') +
    scale_x_date(date_breaks = "2 months", date_labels =  "%b %Y")

}

#' @param vui21oct01
#' @param vui21oct01
PredictionsVUI21OCT01 <- function (vui21oct01, estimates) {

  T <- vui21oct01 %>%
    mutate(series = 'original')
  names(T) <- c('date', 'fraction', 'series')

  P <- estimates %>%
    select(date, prediction) %>%
    mutate(series = 'prediction')
  names(P) <- c('date', 'fraction', 'series')

  timeseries <- rbind(T, P)
  timeseries$series <- factor(timeseries$series, levels = c('original', 'prediction'))

  ggplot(data = timeseries, mapping = aes(x = date, y = fraction, colour = series)) +
    geom_line(alpha = 0.55) +
    geom_point(alpha = 0.15, size = 1) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 11, angle = 90),
          axis.text.y = element_text(size = 11),
          axis.title.x = element_text(face = 'bold', size = 13),
          axis.title.y = element_text(face = 'bold', size = 13),
          legend.title = element_text(),
          panel.grid.minor = element_blank(), panel.grid.major = element_line(size = 0.15)) +
    xlab('\n') +
    ylab('variant fraction\n') +
    scale_x_date(date_breaks = "2 months", date_labels =  "%b %Y") +
    scale_colour_manual(values = c('black', 'orange')) +
    guides(col = guide_legend(title = 'VUI.21OCT.01\nFractions'))

}