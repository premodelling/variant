# Title     : Graphs Delta
# Objective : Illustrate the relative progression of Delta variant infections over time
# Created by: Think
# Created on: 20 November 2021


#' GraphsDelta
#'
#' @description Illustrates the relative progression of Delta variant infections over time
#'
#' @param local: the data frame of non-travel variants data
#'
GraphsDelta <- function (local) {

  # caption
  maxima <- local[which.max(local$Delta), c('week', 'Delta')]
  caption <- str_glue("The weekly progression of Delta variant infections over time.  Each week's Delta fraction
  is a fraction of the week's total non-travel infections due to all recorded variants; named & other.  Delta's
   fractions peak at {first(x = maxima$Delta)} on {first(x = maxima$week)}; hereafter, a slow decline seems
   to have started.")

  # graph, ggtitle( ..., subtitle = '...')
  local %>%
    select(week, Delta) %>%
    ggplot(mapping = aes(x = week, y = Delta)) +
    geom_line(size = 0.15) +
    geom_point(size = 0.75) +
    theme_minimal()  +
    theme(plot.title = element_text(size=13, hjust = 0.5),
          plot.subtitle = element_text(size = 11, colour = 'darkgrey'),
          plot.caption = element_text(hjust = 0, size = 11, colour = 'darkgrey'),
          axis.text.x = element_text(size = 11, angle = 90),
          axis.text.y = element_text(size = 11),
          axis.title.x = element_text(face = 'bold', size = 13),
          axis.title.y = element_text(face = 'bold', size = 13),
          legend.title = element_text(),
          panel.grid.minor = element_blank(), panel.grid.major = element_line(size = 0.15)) +
    xlab('\n') +
    ylab('Delta Variant Fraction\n') +
    labs(caption = str_wrap(caption, width = 100) ) +
    # ggtitle(label = '\nThe Weekly Non-Travel Delta Variant Infections Fractions Over Time\n') +
    scale_x_date(date_breaks = "4 weeks", date_labels =  "%d %b %Y")

}