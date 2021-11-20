# Title     : TODO
# Objective : TODO
# Created by: Think
# Created on: 20/11/2021


StacksTravelYes <- function (stacksdata) {

  stacksdata %>%
    filter(travel == 'travel') %>%
    ggplot(mapping = aes(x = week, y = proportion), alpha = 0.25) +
    geom_col(aes(fill = segment), alpha = 0.60) +
    theme_minimal() +
    xlab('\n') +
    ylab('Variant Proportion\n') +
    scale_x_date(date_breaks = "8 weeks", date_labels =  "%d %b %Y") +
    scale_fill_discrete(name = 'Variant')

}

StacksTravelNo <- function (stacksdata) {

  stacksdata %>%
    filter(travel == 'none') %>%
    ggplot(mapping = aes(x = week, y = proportion), alpha = 0.25) +
    geom_col(aes(fill = segment), alpha = 0.60) +
    theme_minimal() +
    xlab('\n') +
    ylab('Variant Proportion\n') +
    scale_x_date(date_breaks = "8 weeks", date_labels =  "%d %b %Y") +
    scale_fill_discrete(name = 'Variant')

}