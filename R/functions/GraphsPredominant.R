# Title     : TODO
# Objective : TODO
# Created by: Think
# Created on: 20/11/2021

PredominantVariants <- function (predominant) {

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

}

PredominantVariantsZoom <- function (predominant) {

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

}