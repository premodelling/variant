# Title     : Variant Progression Model
# Objective : Predict fractional contributions per day
# Created by: Think
# Created on: 20/11/2021

#' @param starting: The predictions calculation start date; class Date.
#' @param period: The length of time - from 'starting' onwards -  for which predictions are calculated.  The
#'                value must be a lubridate::years(), lubridate::weeks(), or lubridate::days() value
VariantProgressionModel <- function (starting, period) {

  # inspection
  if ( !( period %in% c(lubridate::years(), lubridate::weeks(), lubridate::days()) ) ) {
    stop('The calculation period must be a lubridate::years(), lubridate::weeks(), or lubridate::days() period')
  }


  # ... the model parameters
  beta <- 0.0277
  alpha <- -526.0385


  # ... the prediction equation
  p <- function (d) {
    return(exp(x = d * beta + alpha))
  }


  # ... as.numeric(as.POSIXct(x = as.Date('2021-09-01'), format = '%Y-%m-%d')) / 86400
  estimates <- data.frame(date = seq(from = starting, to = starting + period, by = 'days') )
  estimates <- estimates %>%
    mutate(seconds = as.POSIXct(x = date, format = '%Y-%m-%d')) %>%
    mutate(days = lubridate::day(lubridate::seconds_to_period(seconds))) %>%
    mutate(prediction = as.numeric(lapply(X = days, FUN = p)))

  return(estimates)

}