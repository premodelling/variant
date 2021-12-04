# Title     : Variant
# Objective : Variant
# Created by: 36112985
# Created on: 09/11/2021



#' Functions
source(file = 'R/functions/VariantsData.R')
source(file = 'R/functions/VariantProgressionModel.R')
source(file = 'R/functions/VariantsProportions.R')
source(file = 'R/functions/GraphsDelta.R')
source(file = 'R/functions/GraphsPredominant.R')
source(file = 'R/functions/GraphsStacks.R')
source(file = 'R/functions/GraphsVUI21OCT01.R')



#' Section 2
#' Variants Data

# Preparation: reading-in, ascertaining field data types and/or contents
variants <- VariantsData()
str(object = variants)

variants[, 1:floor(ncol(variants)/2)] %>%
  tibble() %>%
  head(n = 3)


# The melted form of 'variants'
melted <- variants %>%
  tidyr::gather(key = 'variant', value = 'fraction', -c(week, travel))

# variant types, including 'Other'
types <- names(x = variants)
types <- types[ !(types %in% c('week', 'travel')) ]


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
proportions <- VariantsProportions(melted = melted)
names(proportions)
proportions$none
proportions$travel
proportions$leading



# 4.1
# GraphsPredominant
PredominantVariants(predominant = proportions$leadingseries)
PredominantVariantsZoom(predominant = proportions$leadingseries)


# 4.2
# The data for stacks of variant proportions, and the graphs
stacksdata <- melted %>%
  mutate(segment = mapply(function (element, reference) if (element %in% reference) {element} else {'Other'},
                          element =  variant, MoreArgs = list('reference' = proportions$leading$variant))) %>%
  group_by(week, travel, segment) %>%
  summarise(proportion = sum(fraction), .groups = 'keep')
StacksTravelYes(stacksdata = stacksdata)
StacksTravelNo(stacksdata = stacksdata)


# 4.3
# ... description of the bar graphs (4.3)
# Observations:



#' 5
#' New Variant Increase

variants %>%
  select(week, travel, "VUI.21OCT.01") %>%
  filter(travel == 'none' & `VUI.21OCT.01` > 0) %>%
  slice(which.min(week))

variants %>%
  select(week, travel, "VUI.21OCT.01") %>%
  filter( (travel == 'travel' & `VUI.21OCT.01` > 0) ) %>%
  slice(which.min(week))


# VUI.21OCT.01 Data
vui21oct01 <- variants %>%
  filter(travel == 'none') %>%
  select(week, 'VUI.21OCT.01')

# Progression over time
ProgressionVUI21OCT01(vui21oct01 = vui21oct01)

# The variant progression model, and its predictions
starting <- min(vui21oct01$week)
period <- lubridate::years(x = 1)
estimates <- VariantProgressionModel(starting = starting, period = period)

# 1 September 2021
estimates[estimates$date == '2021-09-01', c('date', 'prediction')]

# Graph: VUI.21OCT.01 & predictions
PredictionsVUI21OCT01(vui21oct01 = vui21oct01, estimates = estimates)

# The first day of 2022
estimates[estimates$date == '2022-01-01', c('date', 'prediction')]

# Or
selections <- rbind(estimates[estimates$date == '2021-09-01', c('date', 'prediction')],
                    estimates[estimates$date == '2022-01-01', c('date', 'prediction')])

