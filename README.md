# STAT1560Lab11
STAT1560 Lab11

The first function is to estimate arrival rates using a non-homogeneous poisson process (NHPP).
The second function simulates arrivals with a non-homogeneous poisson process (NHPP).

## Running an Example
To use the functions, source bike_functions.R, load the bike-share dataset, estimate hourly arrival rates, and simulate arrivals using those rates.
source("bike_functions.R")

bikes <- readr::read_csv("../../Data/sample_bike.csv") %>% select(-1)
rates <- estimate_arrival_rates(bikes, "2022-02-16", "2022-02-22")
arrivals <- simulate_arrivals(rates, "2022-02-22", "2022-02-23")
head(arrivals)