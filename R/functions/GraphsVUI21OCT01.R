# Title     : Graphs VUI.21.OCT.01
# Objective : Graphs for Variant Under Investigation VUI.21.OCT.01
# Created by: 36112985
# Created on: 27/11/2021

#' @param vui21oct01:
ProgressionVUI21OCT01 <- function (vui21oct01) {

  plot(vui21oct01$week, vui21oct01$VUI.21OCT.01,
       type = "l", lty = 'solid', col = 'black', lwd = 1.0,
       frame.plot = FALSE,
       xlab = '', ylab = 'proportion', main = 'VUI.21OCT.01')


}

#' @param vui21oct01
#' @param vui21oct01
PredictionsVUI21OCT01 <- function (vui21oct01, estimates) {

  plot(x = vui21oct01$week, y = vui21oct01$VUI.21OCT.01,
       ylim=c(min(vui21oct01$VUI.21OCT.01, estimates$prediction), max(vui21oct01$VUI.21OCT.01, estimates$prediction)),
       xlim = c(min(vui21oct01$week, estimates$date), max(vui21oct01$week, estimates$date)),
       type = "l", lty = 'solid', col = 'black', lwd = 2.5,
       frame.plot = FALSE,
       xlab = '', ylab = 'proportion', main = 'VUI.21OCT.01')
  points(x = estimates$date, y = estimates$prediction,
         pch = 19, cex = 0.5,
         col = scales::alpha(colour = 'grey', alpha = 0.25))

}