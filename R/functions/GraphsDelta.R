# Title     : Graphs Delta
# Objective : Illustrate the relative progression of Delta variant infections over time
# Created by: 36112985
# Created on: 20 November 2021


#' GraphsDelta
#'
#' @description Illustrates the relative progression of Delta variant infections over time
#'
#' @param local: the data frame of non-travel variants data
#'
#' @note Draws the Delta non-travel infections graph
#'
GraphsDelta <- function (local) {

  # caption
  maxima <- local[which.max(local$Delta), c('week', 'Delta')]
  caption <- str_glue("The weekly progression of Delta variant infections over time.  Each week's Delta fraction
  is a fraction of the week's total non-travel infections due to all recorded variants; named & other.  Delta's
   fractions peak at {round(first(x = maxima$Delta), digits = 3)} on {first(x = maxima$week)}; hereafter, a slow decline seems
   to have started.")

  # graph, ggtitle( label = '...', subtitle = '...')
  local %>%
    select(week, Delta) %>%
    ggplot(mapping = aes(x = week, y = Delta)) +
    geom_line(size = 0.15) +
    geom_point(size = 0.75) +
    geom_vline(xintercept = first(x = maxima$week)) +
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
    labs(caption = str_wrap(caption, width = 80) ) +
    scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
    annotate('text', x = first(x = maxima$week), y = 0.25, label = paste('Peak week\n', first(x = maxima$week)))

}