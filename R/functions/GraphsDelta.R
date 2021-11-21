# Title     : TODO
# Objective : TODO
# Created by: Think
# Created on: 20/11/2021

GraphsDelta <- function (local) {

  # Add title & caption
  local %>%
    select(week, Delta) %>%
    ggplot(mapping = aes(x = week, y = Delta)) +
    geom_line(size = 0.15) +
    geom_point(size = 0.75) +
    theme_minimal()  +
    theme(plot.title = element_text(size=15, face = 'bold', hjust = 0.5),
          axis.text.x = element_text(size = 11, angle = 90),
          axis.text.y = element_text(size = 11),
          axis.title.x = element_text(face = 'bold', size = 13),
          axis.title.y = element_text(face = 'bold', size = 13),
          legend.title = element_text(),
          panel.grid.minor = element_blank(), panel.grid.major = element_line(size = 0.15)) +
    xlab('\n') +
    ylab('Delta Variant Proportion\n') +
    ggtitle(label = '\nDelta Variant Infections Over Time\n') +
    scale_x_date(date_breaks = "4 weeks", date_labels =  "%d %b %Y")

}