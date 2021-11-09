# Title     : Variant
# Objective : Variant
# Created by: Think
# Created on: 09/11/2021

variants <- read.csv(file = 'data/variant_travel.csv')
str(variants)

variants$week <- as.Date(variants$week)
str(variants)

# Does the travel field have the elements 'none' & 'travel' only?  (... lower case)
variants$travel <- tolower(variants$travel)
all(unique(variants$travel) %in% c('none', 'travel'))



# Ascertain that per record, the sum of classification fractions = 1
fractions <- variants %>%
  dplyr::select(!c(week, travel)) %>%
  rowSums(na.rm = FALSE, dims = 1)