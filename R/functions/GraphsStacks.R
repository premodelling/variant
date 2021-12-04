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
    theme(axis.text.x = element_text(size = 11, angle = 90),
          axis.text.y = element_text(size = 11),
          axis.title.x = element_text(face = 'bold', size = 13),
          axis.title.y = element_text(face = 'bold', size = 13),
          legend.title = element_text(),
          panel.grid.minor = element_blank(), panel.grid.major = element_line(size = 0.15)) +
    xlab('\n') +
    ylab('Variant Proportion\n') +
    scale_x_date(date_breaks = "8 weeks", date_labels =  "%b %Y") +
    scale_fill_discrete(name = 'Variant')

}

StacksTravelNo <- function (stacksdata) {

  stacksdata %>%
    filter(travel == 'none') %>%
    ggplot(mapping = aes(x = week, y = proportion), alpha = 0.25) +
    geom_col(aes(fill = segment), alpha = 0.60) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 11, angle = 90),
          axis.text.y = element_text(size = 11),
          axis.title.x = element_text(face = 'bold', size = 13),
          axis.title.y = element_text(face = 'bold', size = 13),
          legend.title = element_text(),
          panel.grid.minor = element_blank(), panel.grid.major = element_line(size = 0.15)) +
    xlab('\n') +
    ylab('Variant Proportion\n') +
    scale_x_date(date_breaks = "8 weeks", date_labels =  "%b %Y") +
    scale_fill_discrete(name = 'Variant')

}