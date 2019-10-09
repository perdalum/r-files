#' Calendar Heatmap
#' 
#' Creates a colour coded calendar visualising time series data. 
#' 
#' The implementation is inspired by https://dominikkoch.github.io/Calendar-Heatmap/
#' 
#' @param dates A vector containing the dates in `Date` format.
#' @param values A vector containing the corresponding values as numeric.
#' @param legendtitle Legend title (optional).
#'   
#' @return ggplot object
#'  
calendar_heatmap_continuous_beta <- function(dates, values, legendtitle = ""){

  # load required packages
  library(ggplot2)
  library(tidyverse)
  library(lubridate)
  
    
  # Parameter checks
  if(missing(dates)){
    stop("Need to specify a dates vector.")
  }
  if(missing(values)){
    stop("Need to specify a values vector.")
  }
  if(!is.Date(dates)){
    stop("dates vector need to be in Date format.")
  }
  if(length(dates) != length(values)){
    stop("dates and values need to have the same length.")
  }
  
  

  
  my_theme <- function() {
    
    # Colors
    color.background = "white"
    color.text = "#22211d"
    
    # Begin construction of chart
    theme_bw(base_size=15) +
      
      # Format background colors
      theme(panel.background = element_rect(fill=color.background, color=color.background)) +
      theme(plot.background  = element_rect(fill=color.background, color=color.background)) +
      theme(panel.border     = element_rect(color=color.background)) +
      theme(strip.background = element_rect(fill=color.background, color=color.background)) +
      
      # Format the grid
      theme(panel.grid.major = element_blank()) +
      theme(panel.grid.minor = element_blank()) +
      theme(axis.ticks       = element_blank()) +
      
      # Format the legend
      theme(legend.position = "bottom") +
      theme(legend.text = element_text(size = 8, color = color.text)) +
      theme(legend.title = element_text(size = 10, face = "bold", color = color.text)) +
      
      # Format title and axis labels
      theme(plot.title       = element_text(color=color.text, size=20, face = "bold")) +
      theme(axis.text.x      = element_text(size=12, color="black")) +
      theme(axis.text.y      = element_text(size=12, color="black")) +
      theme(axis.title.x     = element_text(size=14, color="black", face = "bold")) +
      theme(axis.title.y     = element_text(size=14, color="black", vjust=1.25)) +
      theme(axis.text.x      = element_text(size=10, hjust = 0, color = color.text)) +
      theme(axis.text.y      = element_text(size=10, color = color.text)) +
      theme(strip.text       = element_text(face = "bold")) + 
      
      # Plot margins
      theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
  }
  
  # create empty calendar
  min_date <- floor_date(min(dates), "years")
  max_date <- ceiling_date(max(dates), "years") - 1
  
  # fill in values
 tibble(
   # This date column ranges from the beginning of the year of the oldest
   # date to the end of the of of the younges date
   date = seq(
     floor_date(min(dates), "years"),
     ceiling_date(max(dates), "years") - 1,
     by="days")) %>%
   
   # join that tibble with a tibble containing the actual dates and values
   left_join(tibble(date = dates, value = values), by = "date") %>% 
   
   # add some helper values
   mutate(
      year = as_factor(year(date)),
      month = month(date),
      day_of_year = yday(date),
      day_of_week = fct_rev(wday(date, week_start = 1, label = TRUE, abbr = FALSE)),
      week_of_year = isoweek(date)) %>% 
   mutate(week_of_year = if_else(week_of_year == 1 & month == 12, 53, week_of_year)) -> calendar
 
  g <- ggplot(calendar, aes(week_of_year, day_of_week, fill = value)) + 
    geom_tile(colour = "darkgrey") + 
    facet_wrap(~year, ncol = 1) + # Facet for years
    coord_equal(xlim = c(2.5,54)) + # square tiles
    scale_x_continuous(
      breaks = 53/12*(1:12)-3.5,
      labels = 1:12 %>% map(~as.character(month(.x, label = TRUE))) %>% as_vector()) + 
    my_theme() +
    scale_fill_continuous(
      na.value = "white",
      name = legendtitle) +
    labs(x = NULL, 
         y = NULL)
  
  left_join(
    calendar %>% 
      group_by(month) %>%
      summarise(start_day = min(day_of_year)),
    calendar, by = "month") %>% 
    filter(start_day == day_of_year) %>% 
    mutate(
      A_x = week_of_year, 
      B_y = as.numeric(day_of_week)) %>% 
    select(year, month, A_x, B_y) %>% 
    mutate(
      x11 = A_x,     y11 =   7, x21 = A_x,     y21 = B_y,
      x21 = A_x,     y21 = B_y, x22 = A_x - 1, y22 = B_y,
      x31 = A_x - 1, y31 = B_y, x32 = A_x - 1, y32 = 0,
    ) %>% select(-A_x, -B_y) -> coordinates
  
  my_lines<-tibble(
    x=numeric(), 
    y=numeric(), 
    xend=numeric(), 
    yend=numeric(), 
    year=character())
  
  for (this_year in (coordinates %>% select(year) %>% unique() %>% as_vector())) {
    
  for (j in 1:12)  {
    # Unpack the coordinates
    row <- coordinates %>% filter(year == this_year, month == j)
    
    my_lines <- rbind(my_lines,
                      tibble(
                        x    = row %>% select(x11, x21, x31) %>% as_vector() %>% as.numeric() + 0.5,
                        y    = row %>% select(y11, y21, y31) %>% as_vector() %>% as.numeric() + 0.5,
                        xend = row %>% select(x21, x22, x32) %>% as_vector() %>% as.numeric() + 0.5,
                        yend = row %>% select(y21, y22, y32) %>% as_vector() %>% as.numeric() + 0.5,
                        year = this_year))
  }
  
  # End of December
  calendar %>% filter(year == this_year) %>% 
    arrange(day_of_year) %>% 
    filter(row_number() == n()) %>% 
    select(day_of_week, week_of_year) %>% 
    as_vector() -> last_day
  
  if (last_day["week_of_year"] == 1) last_day["week_of_year"] <- 53
  
  A_x <- last_day["week_of_year"]
  B_y <- as.numeric(last_day["day_of_week"]) - 1
  
  my_lines <- rbind(my_lines,
                    tibble(
                      x    = c(A_x, A_x,     A_x - 1) + 0.5,
                      y    = c(7,   B_y,     B_y) + 0.5,
                      xend = c(A_x, A_x - 1, A_x - 1) + 0.5,
                      yend = c(B_y, B_y,     0) + 0.5,
                      year = this_year
                    ))
  
  }
  #   add lines
  g <- g + geom_segment(
    data=my_lines,
    aes(x,y,xend=xend, yend=yend),
    lineend = "square",
    color = "black",
    inherit.aes=FALSE)
  
  return(g)
}
