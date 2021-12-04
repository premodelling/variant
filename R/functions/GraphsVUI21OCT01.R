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

  plot(x = vui21oct01$week, y = vui21oct01$VUI.21OCT.01,
       ylim=c(min(vui21oct01$VUI.21OCT.01, estimates$prediction), max(vui21oct01$VUI.21OCT.01, estimates$prediction)),
       xlim = c(min(vui21oct01$week, estimates$date), max(vui21oct01$week, estimates$date)),
       type = "l", lty = 'solid', col = 'black', lwd = 2.5,
       frame.plot = FALSE,
       xlab = '', ylab = 'fraction', main = 'VUI.21OCT.01 Infection Fractions Over Time\n(non-travel)')
  points(x = estimates$date, y = estimates$prediction,
         pch = 19, cex = 0.5,
         col = scales::alpha(colour = 'green', alpha = 0.25))

}