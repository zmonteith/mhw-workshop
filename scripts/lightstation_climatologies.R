# MHW climatologies
# clear workspace
rm(list = ls())

# # install packages as necessary
# install packages
# install.packages("dplyr")
# install.packages("lubridate")
# install.packages("ggplot2")
# install.packages("tidyr")
# install.packages("reshape2")
# install.packages("wql")
# install.packages("heatwaveR")

# load packages
lapply(c("dplyr", "lubridate", "ggplot2", "tidyr", "reshape2", "wql",
         "heatwaveR"), library, character.only = TRUE)

# load data
## light station data [found here](https://open.canada.ca/data/en/dataset/719955f2-bf8e-44f7-bc26-6bd623e82884)
## Closest active light station surface temperature station is Chrome Island (South of Denman)

# McInnes Island surface temp and salinity
mcinnes <- 
  read.csv("raw-data/mcinnes_daily_sst_salinity.csv", skip = 1,
           col.names = c("t", "salinity_pss", "temp",
                         "lat_dd", "long_dd"),
           na.strings = 999.9) %>%
  mutate(t = ymd(t)) 

# Egg Island surface temp and salinity
egg <- 
  read.csv("raw-data/egg_daily_sst_salinity.csv", skip = 1,
           col.names = c("t", "salinity_pss", "temp",
                         "lat_dd", "long_dd"),
           na.strings = 999.9) %>%
  mutate(t = ymd(t)) 


# make climatology time series light station surface temp
## mcinnes
temp_mcinnes <- 
  ts2clm(data = mcinnes,
         # climatologyPeriod = c("1954-01-01", "2023-06-30"))
         # climatology period should be full years to prevent unequal weightings
         climatologyPeriod = c("1954-01-01", "2022-12-31")) 

# egg
temp_egg <- ts2clm(data = egg,
                   # climatologyPeriod = c("1954-01-01", "2023-06-30"))
                   # climatology period should be full years to prevent unequal weightings
                   climatologyPeriod = c("1970-03-10", "2022-03-09")) 


# # Can use same method to make 'climatologies with different variables such as
# #   salinity
# sal_mcinnes <- ts2clm(data = mcinnes,
#                       climatologyPeriod = c("1954-01-01", "2022-12-31"),
#                       y = salinity_pss)

# detect extreme events
## mcinnes
mhw_mcinnes <- detect_event(temp_mcinnes)
mhw_mcinnes$event %>%
  ungroup() %>%
  select(event_no, duration, date_start, date_end, intensity_max,
         intensity_cumulative) %>%
  arrange(-intensity_cumulative) %>%
  head(10)

## egg
mhw_egg <- detect_event(temp_egg)
mhw_egg$event %>%
  ungroup() %>%
  select(event_no, duration, date_start, date_end, intensity_max,
         intensity_cumulative) %>%
  arrange(-intensity_cumulative) %>%
  head(10)

# # testing out with salinity
# sal_events_egg <- detect_event(sal_egg, y = salinity_pss)
# sal_events_egg$event %>%
#   ungroup() %>%
#   select(event_no, duration, date_start, date_end, intensity_max,
#          intensity_cumulative) %>%
#   arrange(-intensity_cumulative) %>%
#   head(10)

# plot
### defaults to cumulative intensity
### different metrics produce different result in different ranking of most
###   extreme event
### default is centered on largest event in timeseries

## mcinnes
event_line(mhw_mcinnes, metric = "intensity_cumulative",
           spread = 1000)

event_line(mhw_mcinnes, metric = "intensity_cumulative",
           start_date = "2019-01-01", end_date = "2024-02-01")

## egg temperature
event_line(mhw_egg, metric = "intensity_cumulative",
           spread = 1000)

# # egg salinity
# event_line(sal_events_egg, metric = "intensity_cumulative",
#            y = salinity_pss)


