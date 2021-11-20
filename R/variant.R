# Title     : Variant
# Objective : Variant
# Created by: Think
# Created on: 09/11/2021



#' Fuctions
source(file = 'R/functions/PrepareData.R')



#' Following on from Section 2
#' Variants Data

# Preparation
variants <- PrepareData()
str(object = variants)



#' Section 3
#' Non Travel Data Only

# None travel data
local <- variants %>%
  dplyr::filter(travel == 'none')

# Delta proportions over time
# Add title & caption
local %>%
  select(week, Delta) %>%
  ggplot(mapping = aes(x = week, y = Delta)) +
  geom_line(size = 0.15) +
  geom_point(size = 0.75) +
  theme_minimal()  +
  theme(axis.text.x = element_text(size = 11, angle = 90),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(face = 'bold', size = 13),
        axis.title.y = element_text(face = 'bold', size = 13),
        legend.title = element_text(),
        panel.grid.minor = element_blank(), panel.grid.major = element_line(size = 0.15)) +
  xlab('\n') +
  ylab('Delta Variant Proportion\n') +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y")

#' Variant Peaks
peaks <- local %>%
  select(!(travel)) %>%
  tidyr::gather(key = 'variant', value = 'p', -week) %>%
  group_by(variant) %>%
  slice(which.max(p)) %>%
  arrange(desc(p))
peaks



#' Section 4

# The melted form of 'variants'
melted <- variants %>%
  tidyr::gather(key = 'variant', value = 'fraction', -c(week, travel))

# The leading 5 variants reponsible for the most infections during the period in focus
leading <- melted %>%
  filter(variant != 'Other') %>%
  group_by(variant) %>%
  summarise(sum = sum(fraction)) %>%
  slice_max(sum, n = 5)
leading

# Hence, their data
predominant <- dplyr::left_join(x = leading[, 'variant'], y = melted, by = 'variant', keep = FALSE)

# ... the corresponding graph
# ... add caption & title
ggplot(data = predominant, mapping = aes(x = week, y = fraction)) +
  geom_line(aes(colour = variant, linetype = travel)) +
  geom_point(aes(colour = variant), size = 0.75) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 11, angle = 90),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(face = 'bold', size = 13),
        axis.title.y = element_text(face = 'bold', size = 13),
        legend.title = element_text(),
        panel.grid.minor = element_blank(), panel.grid.major = element_line(size = 0.15)) +
  xlab('\n') +
  ylab('Delta Variant Proportion\n') +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y")

ggplot(data = predominant, mapping = aes(x = week, y = fraction)) +
  geom_line(aes(colour = variant, linetype = travel)) +
  geom_point(aes(colour = variant), size = 0.75) +
  ggplot2::coord_trans(y = scales::sqrt_trans()) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 11, angle = 90),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(face = 'bold', size = 13),
        axis.title.y = element_text(face = 'bold', size = 13),
        legend.title = element_text(),
        panel.grid.minor = element_blank(), panel.grid.major = element_line(size = 0.15)) +
  xlab('\n') +
  ylab('Delta Variant Proportion\n') +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y")


# ... stacks
stacksdata <- melted %>%
  mutate(segment = mapply(function (element, reference) if (element %in% reference) {element} else {'Other'},
                          element =  variant, MoreArgs = list('reference' = core$variant)))

stacksdata %>%
  filter(travel == 'travel') %>%
  group_by(week, segment) %>%
  summarise(proportion = sum(fraction)) %>%
  ggplot(mapping = aes(x = week, y = proportion), alpha = 0.25) +
  geom_col(aes(fill = segment), alpha = 0.60) +
  theme_minimal() +
  xlab('\n') +
  ylab('Variant Proportion\n') +
  scale_x_date(date_breaks = "8 weeks", date_labels =  "%d %b %Y") +
  scale_fill_discrete(name = 'Variant')


stacksdata %>%
  filter(travel == 'none') %>%
  group_by(week, segment) %>%
  summarise(proportion = sum(fraction)) %>%
  ggplot(mapping = aes(x = week, y = proportion), alpha = 0.25) +
  geom_col(aes(fill = segment), alpha = 0.60) +
  theme_minimal() +
  xlab('\n') +
  ylab('Variant Proportion\n') +
  scale_x_date(date_breaks = "8 weeks", date_labels =  "%d %b %Y") +
  scale_fill_discrete(name = 'Variant')


# ... description of the bar graphs (4.3)
# ...



#' New Variant Increase
vui21oct01 <- variants %>%
  filter(travel == 'none') %>%
  select(week, 'VUI.21OCT.01')

plot(vui21oct01$week, vui21oct01$VUI.21OCT.01,
     type = "l", lty = 'solid', col = 'black', lwd = 1.0,
     frame.plot = FALSE,
     xlab = '', ylab = 'proportion', main = 'VUI.21OCT.01')


# ... model function, table expansion
starting <- min(vui21oct01$week)
period <- lubridate::years(x = 1)

source('R/functions/VariantProgressionModel.R')

VariantProgressionModel(starting = starting, period = period)


