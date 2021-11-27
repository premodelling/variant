# Title     : Prepare Data
# Objective : Prepare Data
# Created by: Think
# Created on: 18/11/2021

#' Data Get
#'
#' @description reads-in the variants data, and ensures that the
#'
#'          * format of the 'week' field is Data
#'          * travel field consists of 2 elements only: none & travel
#'          * format of the travel field is Factor
#'          * sum of fractions per record is 1
#'
#' @return appropriately formatted, and checked, dataframe of variants data
#'
VariantsData <- function () {

  # Reading-in the data
  variants <- read.csv(file = 'data/variant_travel.csv')

  # Converting field 'week' to Date format
  variants$week <- as.Date(variants$week)

  # Ascertaining that the travel field has the elements 'none' & 'travel' only (... lower case)
  variants$travel <- tolower(variants$travel)
  if ( !all(unique(variants$travel) %in% c('none', 'travel')) ) {
    stop('The travel field has unknown elements')
  }

  # Converting field 'travel' to Factor format
  variants$travel <- factor(x = variants$travel, ordered = FALSE)

  # Ascending date order
  variants %>%
    arrange(week, travel)

  # Ascertaining that the sum of classification fractions = 1 per record
  fractions <- variants %>%
    dplyr::select(!c(week, travel)) %>%
    rowSums(na.rm = FALSE, dims = 1) %>%
    round(digits = 2)
  if ( any(fractions != 1) ) {
    stop('One or more records has a fractions sum that is not equal to one.')
  }

  return(variants)

}
