# Title     : TODO
# Objective : TODO
# Created by: Think
# Created on: 21/11/2021

VariantsProportions <- function (melted) {

  # Preliminaries
  T <- melted %>%
    filter(variant != 'Other') %>%
    group_by(travel, variant) %>%
    summarise(proportion = sum(fraction), .groups = 'keep') %>%
    arrange(travel, desc(proportion))
  T$proportion <- round(T$proportion, digits = 3)
  none <- T[T$travel == 'none', c('variant', 'proportion')]
  travel <- T[T$travel == 'travel', c('variant', 'proportion')]

  # The leading 5 variants reponsible for the most infections during the period the data encompasses
  leading <- melted %>%
    filter(variant != 'Other') %>%
    group_by(variant) %>%
    summarise(sum = sum(fraction)) %>%
    slice_max(sum, n = 5)

  # Hence, the data of the leading 5 variants
  leadingseries <- dplyr::left_join(x = leading[, 'variant'], y = melted, by = 'variant', keep = FALSE)

  return( list('none' = none, 'travel' = travel,
               'leading' = leading, 'leadingseries' = leadingseries) )
}