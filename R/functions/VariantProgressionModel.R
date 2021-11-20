# Title     : TODO
# Objective : TODO
# Created by: Think
# Created on: 20/11/2021


VariantProgressionModel <- function (starting, period) {

  if ( !( period %in% c(lubridate::years(), lubridate::weeks(), lubridate::days()) ) ) {
    stop('The calculation period must be a lubridate::years(), lubridate::weeks(), or lubridate::days() period')
  }


  # ... model
  beta <- 0.0277
  alpha <- -526.0385

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