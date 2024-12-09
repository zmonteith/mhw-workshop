# Seagrass resiliency
## Analyzing EBOV responses to an increasing dynamic environment

# clear workspace
rm(list = ls())

# load packages
lapply(c("dplyr", "lubridate", "ggplot2", "tidyr", "reshape2", "wql",
         "heatwaveR"), library, character.only = TRUE)

# Set working directory directly in console to your file locations. The rest of
#   this script uses relative paths, so this is an essential step for the code
#   to work.
# setwd("/YOUR/FILE/LOCATIONS")

# check working directory not working
# getwd()

# Load source files
## temperature data
source("scripts/tempLoggersNearshore.R")
source("scripts/mcinnes_temperature.R")
glimpse(temperature)

# separate out date and temp only
temp <- temperature %>%
  filter(measure == "Med") %>%
  select(site, t = dateTime, temp = temp.degC) %>%
  mutate(t = as.Date(t))

## split into sites
temp_flat <- temp[temp$site == "Choked_flat_sgrass", ]
temp_inner <- temp[temp$site == "Choked_inner_sgrass", ]
temp_wolf <- temp[temp$site == "Wolf_beach_fish", ]
temp_tbay <- temp[temp$site == "Triquet_bay01", ]
temp_kmid <- temp[temp$site == "Koeye_mid02", ]
temp_tnorth <- temp[temp$site == "Triquet_north_sgrass", ]

## make climatology ts
tempTs_flat <- ts2clm(data = temp_flat,
                      climatologyPeriod = c("2015-09-20", "2022-10-20"))
tempTs_inner <- ts2clm(data = temp_inner,
                      climatologyPeriod = c("2015-09-20", "2022-10-20"))
tempTs_wolf <- ts2clm(data = temp_wolf,
                      climatologyPeriod = c("2015-09-20", "2022-10-20"))
tempTs_tbay <- ts2clm(data = temp_tbay,
                      climatologyPeriod = c("2015-09-20", "2022-10-20"))
tempTs_kmid <- ts2clm(data = temp_kmid,
                      climatologyPeriod = c("2015-09-20", "2022-10-20"))
tempTs_tnorth <- ts2clm(data = temp_tnorth,
                      climatologyPeriod = c("2015-09-20", "2022-10-20"))

## detect MHW events
mhw_flat <- detect_event(tempTs_flat)
mhw_inner <- detect_event(tempTs_inner)
mhw_wolf <- detect_event(tempTs_wolf)
mhw_tbay <- detect_event(tempTs_tbay)
mhw_kmid <- detect_event(tempTs_kmid)
mhw_tnorth <- detect_event(tempTs_tnorth)


## default event plot (max MHW event window)
event_line(mhw_flat, spread = 1000, metric = "intensity_max",
           start_date = "2015-01-01", end_date = "2017-01-01")
event_line(mhw_inner, spread = 1000, metric = "intensity_max",
           start_date = "2015-01-01", end_date = "2017-01-01")
event_line(mhw_wolf, spread = 1000, metric = "intensity_max",
           start_date = "2015-01-01", end_date = "2017-01-01")
event_line(mhw_tbay, spread = 700, metric = "intensity_max",
           start_date = "2015-12-01", end_date = "2017-01-01")
event_line(mhw_kmid, spread = 700, metric = "intensity_max",
           start_date = "2015-12-01", end_date = "2017-01-01")
event_line(mhw_tnorth, spread = 2000, metric = "intensity_max",
           start_date = "2015-12-01", end_date = "2017-01-01")


# Combine McInnes with Choked Inner temp
## Compare data frames
glimpse(temp_mcinnes)
glimpse(temp_wolf)

## aggregate flat to daily mean to match with mcinnes
temp_wolf_agg <- temp_wolf %>%
  group_by(t) %>%
  summarise(temp = mean(temp),
            var = var(temp)) %>%
  rename(temp_w = temp)

## add site to mcinnes
temp_mcinnes_agg <- temp_mcinnes %>%
  rename(temp_mc = temp)

temp_combined <- right_join(temp_mcinnes_agg, temp_wolf_agg)

ggplot(data = temp_combined) +
  geom_density(aes(x = resid)) +
  theme_classic() +
  geom_vline(xintercept = 0, lty = 2)

ggplot(data = temp_combined) +
  geom_point(aes(x = temp_w, y = temp_mc,
                 col = month(t))) +
  geom_abline(intercept = 0, slope = 1) +
  
  theme_classic()

temp_m1 <- glm(temp_mc ~ temp_w, data = temp_combined)
summary(temp_m1)
glimpse(temp_m1)
plot(temp_m1)


## merge flat with mcinnes to fill gaps
temp_combined <- right_join(temp_mcinnes_agg, temp_wolf_agg) %>%
  mutate(resid = temp_w - temp_mc)

temp_wolf_interp <- temp_wolf %>%
  group_by(t) %>%
  summarise(temp = mean(temp),
            var = var(temp))

temp_interp <- left_join(temp_mcinnes, temp_wolf_interp)

# make climatology ts
temp_mcinnes_int <- ts2clm(data = temp_interp,
                       # climatologyPeriod = c("1954-01-01", "2023-06-30"))
                       # climatology period should be full years to prevent unequal weightings
                       climatologyPeriod = c("1954-01-01", "2022-12-31")) 

# detect events
mhw_mcinnes_int <- detect_event(temp_mcinnes_int)
mhw_mcinnes_int$event %>%
  ungroup() %>%
  select(event_no, duration, date_start, date_end, intensity_max,
         intensity_cumulative) %>%
  arrange(-intensity_cumulative) %>%
  head(10)

# plot
event_line(mhw_mcinnes_int, metric = "intensity_cumulative")

##############################


# Flat Islands
## make climatology series from daily timeseries
### heatwaves
tempTs_flat <- ts2clm(data = temp_flat,
                 climatologyPeriod = c("2015-09-20", "2022-10-20"))
### cold spells
tempTs_flat_cold <- ts2clm(data = temp_flat,
                           climatologyPeriod = c("2015-09-20", "2022-10-20"),
                           pctile = 10)

## detect heatwave events
mhw_flat <- detect_event(tempTs_flat)
### look at top heatwave events
mhw_flat$event %>%
  ungroup() %>%
  select(event_no, duration, date_start, date_peak, intensity_max,
         intensity_cumulative) %>%
  arrange(-intensity_cumulative) %>%
  head()
## detect cold spell events
mcs_flat <- detect_event(tempTs_flat_cold, coldSpells = TRUE)
### look at top cold spell events
mcs_flat$event %>%
  ungroup() %>%
  select(event_no, duration, date_start, date_peak, intensity_mean,
         intensity_max, intensity_cumulative) %>%
  arrange(intensity_cumulative) %>%
  head()

## plot 2016
### large heatwave event in fall 2016
event_line(mhw_flat, spread = 200, metric = "intensity_max",
           start_date = "2015-12-01", end_date = "2017-01-01")
## plot 2017
### minimal/no heatwave events in 2017
event_line(mhw_flat, spread = 200, metric = "intensity_max",
           start_date = "2016-12-01", end_date = "2018-01-01")
## plot 2018
### no heatwave event in 2018, but cold late summer, see cold spell below
event_line(mhw_flat, spread = 200, metric = "intensity_max",
           start_date = "2017-12-01", end_date = "2019-01-01")
### 2018 cold spell
event_line(mcs_flat, spread = 200, metric = "intensity_cumulative",
           start_date = "2017-12-01", end_date = "2019-01-01")
## plot 2019
### late winter, spring heat events
event_line(mhw_flat, spread = 200, metric = "intensity_max",
           start_date = "2018-12-01", end_date = "2020-01-01")
## 2019 cold events
event_line(mcs_flat, spread = 200, metric = "intensity_cumulative",
           start_date = "2018-12-01", end_date = "2020-01-01")
## plot 2020
### multiple mhw events in summer, fall
event_line(mhw_flat, spread = 200, metric = "intensity_max",
           start_date = "2019-12-01", end_date = "2021-01-01")
## plot 2021
### no mhw events
event_line(mhw_flat, spread = 200, metric = "intensity_max",
           start_date = "2020-12-01", end_date = "2022-01-01")
## 2021 cold events
### cold year overall, mcs lasting nearly entire year
event_line(mcs_flat, spread = 200, metric = "intensity_cumulative",
           start_date = "2020-12-01", end_date = "2022-01-01")
## plot 2022
### temperature variable through summer, single short mhw
event_line(mhw_flat, spread = 200, metric = "intensity_max",
           start_date = "2021-12-01", end_date = "2023-01-01")
## 2022 cold events
### cold summer, especially cold fall/early winter
event_line(mcs_flat, spread = 200, metric = "intensity_cumulative",
           start_date = "2021-12-01", end_date = "2023-01-01")
## plot 2023
### spring and fall mhw, relatively small
event_line(mhw_flat, spread = 200, metric = "intensity_max",
           start_date = "2022-12-01", end_date = "2023-08-09")
## 2023 cold events
### cold spring, straight through summer
event_line(mcs_flat, spread = 200, metric = "intensity_cumulative",
           start_date = "2022-12-01", end_date = "2023-08-09")


# Triquet Bay
## make climatology series from daily timeseries
### heatwaves
tempTs_tbay <- ts2clm(data = temp_tbay,
                      climatologyPeriod = c("2015-09-20", "2022-10-20"))
### cold spells
tempTs_tbay_cold <- ts2clm(data = temp_tbay,
                           climatologyPeriod = c("2015-09-20", "2022-10-20"),
                           pctile = 10)

## detect heatwave events
mhw_tbay <- detect_event(tempTs_tbay)
### look at top heatwave events
mhw_tbay$event %>%
  ungroup() %>%
  select(event_no, duration, date_start, date_peak, intensity_max,
         intensity_cumulative) %>%
  arrange(-intensity_max) %>%
  head()
## detect cold spell events
mcs_tbay <- detect_event(tempTs_tbay_cold, coldSpells = TRUE)
### look at top cold spell events
mcs_tbay$event %>%
  ungroup() %>%
  select(event_no, duration, date_start, date_peak, intensity_mean,
         intensity_max, intensity_cumulative) %>%
  arrange(intensity_cumulative) %>%
  head()

## plot 2016
### some fall heatwaves, low intensity
event_line(mhw_tbay, spread = 200, metric = "intensity_max",
           start_date = "2015-12-01", end_date = "2017-01-01")
### 2016 MCS
event_line(mcs_tbay, spread = 200, metric = "intensity_cumulative",
           start_date = "2015-12-01", end_date = "2017-01-01")
## plot 2017
### data appears missing
event_line(mhw_tbay, spread = 200, metric = "intensity_max",
           start_date = "2016-12-01", end_date = "2018-01-01")
## plot 2018
### n = 2 small, sort MHW in winter
event_line(mhw_tbay, spread = 200, metric = "intensity_max",
           start_date = "2017-12-01", end_date = "2019-01-01")
## 2018 cold spell
### start of year, summer, fall
event_line(mcs_tbay, spread = 200, metric = "intensity_cumulative",
           start_date = "2017-12-01", end_date = "2019-01-01")
## plot 2019
### late winter, spring heat events
event_line(mhw_tbay, spread = 200, metric = "intensity_max",
           start_date = "2018-12-01", end_date = "2020-01-01")
## 2019 cold events
event_line(mcs_tbay, spread = 200, metric = "intensity_cumulative",
           start_date = "2018-12-01", end_date = "2020-01-01")
## plot 2020
### multiple mhw events in summer, fall
event_line(mhw_tbay, spread = 200, metric = "intensity_max",
           start_date = "2019-12-01", end_date = "2021-01-01")
## plot 2021
### no mhw events
event_line(mhw_tbay, spread = 200, metric = "intensity_max",
           start_date = "2020-12-01", end_date = "2022-01-01")
## 2021 cold events
### cold year overall, mcs lasting nearly entire year
event_line(mcs_tbay, spread = 200, metric = "intensity_cumulative",
           start_date = "2020-12-01", end_date = "2022-01-01")
## plot 2022
### temperature variable through summer, single short mhw
event_line(mhw_tbay, spread = 200, metric = "intensity_max",
           start_date = "2021-12-01", end_date = "2023-01-01")
## 2022 cold events
### cold summer, especially cold fall/early winter
event_line(mcs_tbay, spread = 200, metric = "intensity_cumulative",
           start_date = "2021-12-01", end_date = "2023-01-01")
## plot 2023
### spring and fall mhw, relatively small
event_line(mhw_tbay, spread = 200, metric = "intensity_max",
           start_date = "2022-12-01", end_date = "2023-08-09")
## 2023 cold events
### cold spring, straight through summer
event_line(mcs_tbay, spread = 200, metric = "intensity_cumulative",
           start_date = "2022-12-01", end_date = "2023-08-09")



tempTs_inner <- ts2clm(data = temp_inner,
                      climatologyPeriod = c("2015-09-20", "2022-10-20"))
mhw_inner <- detect_event(tempTs_inner)
event_line(mhw_inner, spread = 180, metric = "intensity_max",
           start_date = "2015-09-15", end_date = "2023-08-09")

tempTs_wolf <- ts2clm(data = temp_wolf,
                       climatologyPeriod = c("2015-09-20", "2022-10-20"))
mhw_wolf <- detect_event(tempTs_wolf)
event_line(mhw_wolf, spread = 180, metric = "intensity_max",
           start_date = "2015-09-15", end_date = "2023-08-09")




