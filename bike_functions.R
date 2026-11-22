library(dplyr)
library(tidyr)
library(lubridate)
set.seed(1560)

#' Estimate hourly customer arrival rates
#'
#' Estimates customer arrival rates for each hour, start station, 
#' and end station, adjusting the observed trip rates based on 
#' bike availability at the start station.
#'
#' @param data A data frame containing start_station, end_station,
#'   start_time, and end_time. Rebalancing events are given by "R".
#' @param window_start The start of the observation window.
#' @param window_end The end of the observation window.
#'
#' @return A data frame with one row for each start/end/hour
#'   combination, containing the columns origin, destination,
#'   hour, observed_rate, availability, and rate.
#'   If no trips are observed, observed_rate is 0.
#'   If availability is 0, the estimated rate is NA.
#'   
estimate_arrival_rates <- function(data, window_start, window_end) {
  
  # cleaning the data
  
  data <- data %>%
    mutate(
      start_station = as.character(start_station),
      end_station = as.character(end_station),
      start_time = as.POSIXct(start_time, tz = "UTC"),
      end_time = as.POSIXct(end_time, tz = "UTC")
    )
  
  window_start = as.POSIXct(window_start, tz = "UTC")
  window_end = as.POSIXct(window_end, tz = "UTC")
  
  n_days <- as.numeric(difftime(
    window_end, window_start, units = "days"))
  
  # check for missing values
  if (anyNA(data$start_station) |
      anyNA(data$end_station) |
      anyNA(data$start_time) |
      anyNA(data$end_time)) {
    stop("Missing values in input data")
  }
  
  # check that the window consists of complete days
  if (is.na(n_days) |
      n_days <= 0 |
      n_days != floor(n_days) |
      format(window_start, "%H:%M:%S") != "00:00:00" |
      format(window_end, "%H:%M:%S") != "00:00:00") {
    stop("Observation window must consist of complete days")
  }
  
  # find all stations
  
  all_stations <- unique(c(data$start_station, data$end_station))
  all_stations <- all_stations[all_stations != "R"] # remove rebalancing rows
  
  # check that there is at least one station
  if (length(all_stations) == 0) {
    stop("No stations found in input data")
  }
  
  # calculate x_hat
  
  observed_trips <- data %>%
    filter(start_station != "R", 
           end_station != "R",
           start_time >= window_start,
           start_time < window_end) %>%
    mutate(hour = hour(start_time))  %>%
    group_by(origin = start_station, #s
             destination = end_station, #t
             hour #h
    ) %>%
    summarise(observed_rate = n() / n_days) %>% # x_hat
    ungroup()
  
  # build events table (from week 10)
  
  events <- data %>%
    mutate(
      original_row = row_number(),
      is_rebalancing = 
        start_station == "R" | 
        end_station == "R"
    ) %>%
    pivot_longer(
      cols = c(start_station, end_station),
      names_to = "direction",
      values_to = "station"
    ) %>%
    filter(station != "R")
  
  # initialize event info
  events$time <- events$start_time
  events$inventory_change <- -1
  events$event_type <- "C"
  events$event_order <- 2
  
  for (i in 1:nrow(events)) {
    
    # if arrival
    if (events$direction[i] == "end_station") {
      events$time[i] <- events$end_time[i]
      events$inventory_change[i] <- 1
      events$event_order[i] <- 3
    }
    
    # if rebalancing event
    if (events$is_rebalancing[i]) {
      
      events$event_type[i] <- "R"
      
      if (events$inventory_change[i] == 1) {
        events$event_order[i] <- 1
      } else {
        events$event_order[i] <- 4
      }
    }
  }
  
  # sort and select cols
  event_table <- events %>%
    filter(time < window_end) %>%
    select(station, time, inventory_change, event_type, 
           event_order, original_row)
  
  # insert hourly placeholders
  
  hour_bounds <- seq(from = window_start, to = window_end, by = "hour")
  
  placeholders <- data.frame()
  
  for (s in all_stations) {
    
    station_placeholders <- data.frame(
      station = s,
      time = hour_bounds,
      inventory_change = 0,
      event_type = "P",
      event_order = 5,
      original_row = 0
    )
    
    placeholders <- rbind(placeholders, station_placeholders) # append rows
  }
  
  # add placeholders to the events table
  event_table <- rbind(event_table, placeholders) %>%
    arrange(time, event_order, original_row)
  
  # track inventories (week 10)
  
  counts <- rep(0, length(all_stations))
  names(counts) <- all_stations
  
  history <- event_table
  history$inventory <- integer(nrow(history))
  
  # update station inventories
  for (i in 1:nrow(history)) {
    station <- history$station[i]
    
    counts[station] <- counts[station] +
      history$inventory_change[i]
    
    history$inventory[i] <- counts[station]
  }
  
  # check for negative inventories
  if (any(history$inventory < 0)) {
    stop("Negative station inventory detected")
  }
  
  # find alpha_hat
  
  availability_table <- history %>%
    group_by(station) %>%
    mutate(
      next_avail_time = lead(time),
      duration = as.numeric(difftime(
        next_avail_time, time, units = "mins"
      ))
    ) %>%
    ungroup() %>%
    filter(time >= window_start,
           time < window_end) %>%
    mutate(
      hour = hour(time),
      available_minutes = if_else(inventory > 0, duration, 0)
    ) %>%
    group_by(origin = station, hour) %>%
    summarise(
      availability = sum(available_minutes) / (60 * n_days)
    ) %>%
    ungroup()
  
  # combine x_hat and alpha_hat
  
  all_combinations <- merge(
    availability_table,
    data.frame(destination = all_stations),
    by = NULL
  ) # every combo of s/t/h
  
  rates <- all_combinations %>%
    left_join(
      observed_trips,
      by = c("origin", "destination", "hour")
    ) %>% 
    # adds observed rate col, 
    # NA for trips that are not observed, so change to 0
    mutate(
      observed_rate = if_else(
        is.na(observed_rate), 0, observed_rate
      ),
      rate = if_else(
        availability == 0,
        NA,
        observed_rate / availability
      )
    ) %>%
    select(origin, destination, hour,
           observed_rate, availability, rate) %>%
    arrange(origin, destination, hour)
  
  return(rates)
  
}