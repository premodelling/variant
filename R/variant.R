# Title     : Variant
# Objective : Variant
# Created by: Think
# Created on: 09/11/2021



#' Functions
source(file = 'R/functions/PrepareData.R')
source('R/functions/VariantProgressionModel.R')
source(file = 'R/functions/GraphsDelta.R')
source(file = 'R/functions/GraphsPredominant.R')
source(file = 'R/functions/GraphsStacks.R')



#' Section 2
#' Variants Data

# Preparation: reading-in, ascertaining field data types and/or contents
variants <- PrepareData()
str(object = variants)



#' Section 3
#' Non-Travel Data

# 3.0
# The non-travel data only
local <- variants %>%
  dplyr::filter(travel == 'none')

# 3.1.a
# Delta proportions over time
GraphsDelta(local = local)

# 3.1.b
# Variant Peaks
peaks <- local %>%
  select(!(travel)) %>%
  tidyr::gather(key = 'variant', value = 'p', -week) %>%
  group_by(variant) %>%
  slice(which.max(p)) %>%
  arrange(desc(p))
peaks



#' Section 4
#' Graphs of Variant Proportions by Travel Category

# 4.0
# The melted form of 'variants'
melted <- variants %>%
  tidyr::gather(key = 'variant', value = 'fraction', -c(week, travel))

# The leading 5 variants reponsible for the most infections during the period the data encompasses
leading <- melted %>%
  filter(variant != 'Other') %>%
  group_by(variant) %>%
  summarise(sum = sum(fraction)) %>%
  slice_max(sum, n = 5)
leading

# Hence, their data
predominant <- dplyr::left_join(x = leading[, 'variant'], y = melted, by = 'variant', keep = FALSE)


# 4.1
# GraphsPredominant
PredominantVariants(predominant = predominant)
PredominantVariantsZoom(predominant = predominant)


# 4.2
# The data for stacks of variant proportions, and the graphs
stacksdata <- melted %>%
  mutate(segment = mapply(function (element, reference) if (element %in% reference) {element} else {'Other'},
                          element =  variant, MoreArgs = list('reference' = leading$variant))) %>%
  group_by(week, travel, segment) %>%
  summarise(proportion = sum(fraction), .groups = 'keep')
StacksTravelYes(stacksdata = stacksdata)
StacksTravelNo(stacksdata = stacksdata)


# 4.3
# ... description of the bar graphs (4.3)
# Observations:



#' 5
#' New Variant Increase

# VUI.21OCT.01 Data
vui21oct01 <- variants %>%
  filter(travel == 'none') %>%
  select(week, 'VUI.21OCT.01')

# Progression over time
plot(vui21oct01$week, vui21oct01$VUI.21OCT.01,
     type = "l", lty = 'solid', col = 'black', lwd = 1.0,
     frame.plot = FALSE,
     xlab = '', ylab = 'proportion', main = 'VUI.21OCT.01')

# The variant progression model, and its predictions
starting <- min(vui21oct01$week)
period <- lubridate::years(x = 1)
estimates <- VariantProgressionModel(starting = starting, period = period)

# 1 September 2021
estimates[estimates$date == '2021-09-01', c('date', 'prediction')]

# Graph: VUI.21OCT.01 & predictions
plot(x = vui21oct01$week, y = vui21oct01$VUI.21OCT.01,
     ylim=c(min(vui21oct01$VUI.21OCT.01, estimates$prediction), max(vui21oct01$VUI.21OCT.01, estimates$prediction)),
     xlim = c(min(vui21oct01$week, estimates$date), max(vui21oct01$week, estimates$date)),
     type = "l", lty = 'solid', col = 'black', lwd = 2.5,
     frame.plot = FALSE,
     xlab = '', ylab = 'proportion', main = 'VUI.21OCT.01')
points(x = estimates$date, y = estimates$prediction, pch = 19, cex = 0.5, col = scales::alpha(colour = 'grey', alpha = 0.25))

# The first day of 2022
estimates[estimates$date == '2022-01-01', c('date', 'prediction')]

