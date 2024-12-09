# Temperature logger data

rm(list = ls())
# load packages
lapply(c("dplyr", "lubridate", "ggplot2", "tidyr", "reshape2"), library, character.only = TRUE)

# Set working directory directly in console to your file locations. The rest of
#   this script uses relative paths, so this is an essential step for the code
#   to work.
# setwd("/YOUR/FILE/LOCATIONS")
setwd("/Users/zach/seagrass-resiliency")

# check working directory
getwd()

# Load data
## Read headers in first
temperatureHeaders <- read.csv("raw-data/seagrass_subtidal_temperature.csv",
                               nrows = 1, as.is = TRUE,
                               header = FALSE)
## Read data in, drop redundant header rows
tmp <- read.csv("raw-data/seagrass_subtidal_temperature.csv",
                        header = FALSE,
                        skip = 4,
                        stringsAsFactors = FALSE)
## Add headers to df
colnames(tmp) <- temperatureHeaders
names(tmp)

glimpse(tmp)

# Reformat dataset: 'melt'
tmp2 <- tmp %>% 
  gather(site, temp.degC, - "Measurement time", - Year, - Month, - WaterYear) %>%
  glimpse

# split site and measure from site var
tmp3 <- tmp2 %>% 
  mutate(site    = gsub("TWtr", "", site),
         measure = gsub("^[[:alpha:]]*_?[[:alnum:]]*_?[a-z0-9]*_?", "", site),
         site    = gsub("_?[[:upper:]]{1,6}_?[[:lower:]]*[[:upper:]]?_?[[:lower:]]{0,5}$", "", site),
         dateTime = ymd_hms(`Measurement time`))

tmp4 <- tmp3 %>% filter(measure == c("Med", "Max", "Std"))

# Still needs work, not currently functional; mutate function above is acceptable alternative
# tmp3 <- tmp2 %>%
#   separate(site, c("site", "measure"), 
#            sep = "^[[:alpha:]]{4}([[:alpha:]]*_?[[:alnum:]]|[[:alpha:]]*_?[[:alnum:]]*_?[[a-z0-9]*)_?([[:upper:]]{1}.*)")

# Filter out Q_level and Q_flags, and values > 15degC
temperature <- tmp3 %>%
  mutate(temp.degC = as.numeric(temp.degC)) %>%
  mutate_if(is.numeric, round, 3) %>%
  filter(# temp.degC < 15,
         measure != "Q_level",
         measure != "Q_flags")

# create subsample of temperature data for quicker testing 
tempSub <- temperature[sample(dim(temperature)[1], size = 1000, replace = FALSE), ]



## --- temporarily code out all below until needed --- ##

# #group all Choked and Pruth loggers - ## not sure how to get this run
# recodeSTempSites <- function(df, site) {
#   recode_factor(df[[site]],
#                 "Pruth Bay"     = "Pruth_bay01",  # DB variable names
#                 "Pruth Bay"     = "Pruth_bay02",  # temp loggerdataset names
#                 "Choked Pass"   = "Choked_flat_sgrass",
#                 "Choked Pass"   = "Choked_inner_sgrass",
#                 "Choked Pass"   = "Choked_sandspit_sgrass")
# }
# 
# #or, subset CHoked_sandspit and #Pruth_bay01 - longest time series
# temperature<-subset(temperature, site %in% c("Choked_sandspit_sgrass","Pruth_bay01"))
#                
# # Plot temperature data - basic
# temperature %>% filter(measure == "Avg") %>%
#   ggplot(aes(x = dateTime, y = temp.degC, col = site)) +
#   geom_line() +
#   facet_grid(site ~ .) +
#   theme_classic()
# 
# # Plot temperature data - grouped 
# 
# YLab_temp <- expression("Mean Temperature" ~ (C))
# XLab_date <- expression("Date" ~ (Year-Month))
# 
# temperature %>% filter(measure == "Avg") %>%
#   ggplot(aes(x = dateTime, y = temp.degC, col = site)) +
#   xlab(XLab_date) +
#   ylab(YLab_temp) +
#   geom_line() +
#   ylim(5, 14)+
#   #geom_point() +
#   #geom_smooth(method="loess") + #likely need polynomial fit here
#   #facet_grid(site ~ .) +
#   scale_colour_discrete(name="Site",
#                       labels=c("Choked Pass", "Pruth Bay"))+
#   theme_classic()
# 
# ##############################################
# #trying this package
# #plot moving averages 
# install.packages("RcppRoll")
# lapply(c("RcppRoll"), library, character.only = TRUE)
# library(RcppRoll)
# 
# 
# #group by site to calculate averages separately
# temperature<-group_by(temperature$site)
# 
# mutate(moving_average = roll_mean(dateTime, 30, align="right", fill=0))
# #moving average of 30 days, moving average done on previous ('right') 30 days
# 
# ##############################
# #trying tqmutate
# #https://www.r-bloggers.com/tidy-time-series-analysis-part-2-rolling-functions/
# 
# library(tidyquant)  # Loads tidyverse, tidyquant, financial pkgs, xts/zoo
# library(cranlogs)   # For inspecting package downloads over time
# 
# library(zoo)
# 
# # "roll" functions from zoo
# tq_mutate()
# #can't get this to work
# 
# ############################
# 
# https://stackoverflow.com/questions/48360023/how-to-calculate-8-hour-rolling-mean-on-year-long-dataset-in-r
# 
# library(zoo)
# library(dplyr)
# library(lubridate)
# 
# temperature %>% 
#   mutate(day = as.factor(day(dateTime)),
#          month = as.factor(month(dateTime),
#          year = as.factor(year(dateTime)),
#          rolling_mean = rollmean(.$temp.degC,
#                    k = 24,   #24 hr rolling mean
#                 fill = NA,
#                 align = "center")) %>% 
#            group_by(day, month, year) %>% 
#            summarise(max_day = max(rolling_mean, na.rm = TRUE)) %>% 
#            ungroup()
# 
# ###########################
# 
# https://danieljhocking.wordpress.com/2014/12/03/lags-and-moving-means-in-dplyr/
#   
# # moving mean for that day and previous days (e.g. 5 represents the mean of that day and the for previous days)
# temperature_mean= temperature %>%
#   group_by(site) %>%
#   arrange(site) %>%
#   mutate(temp.mean7 = rollmean(x = temp.degC, 7, align = "right", fill = NA))
# glimpse(temperature_mean)
# 
# # moving mean for the previous days not including the current day (e.g. 7 represents the mean of the 7 previous days)
# temperature_mean = temperature_mean %>%
#   mutate(temp.lag1 = lag(temp.degC, n = 1)) %>% #lag of 1 day
#   mutate(temp.7.previous = rollapply(data = temp.lag1, 
#                                      width = 7, #7 days(i.e. 1 week)
#                                      FUN = mean, 
#                                      align = "right", 
#                                      fill = NA, 
#                                      na.rm = T))
# glimpse(temperature_mean)
# tail(temperature_mean)
# 
# # moving mean for that day and previous month (e.g. 28 previous days)
# temperature_mean= temperature %>%
#   group_by(site) %>%
#   arrange(site) %>%
#   mutate(temp.mean28 = rollmean(x = temp.degC, 28, align = "right", fill = NA))
# 
# 
# # Plot temperature data - 7 day moving average (including today)
# temperature_mean %>% #filter(measure == "Avg") %>%
#   ggplot(aes(x = dateTime, y =  temp.mean7, col = site)) +
#   geom_line() +
#   facet_grid(site ~ .) +
#   theme_classic()
# 
# # Plot temperature data - 7 day moving average (including today)
# temperature_mean %>% #filter(measure == "Avg") %>%
#   ggplot(aes(x = dateTime, y =  temp.7.previous, col = site)) +
#   geom_line() +
#   facet_grid(site ~ .) +
#   theme_classic()
# 
# # Plot temperature data - 7 day moving average (including today)
# temperature_mean %>% filter(measure == "Avg") %>%
#   ggplot(aes(x = dateTime, y =  temp.mean28, col = site)) +
#   geom_line() +
#   facet_grid(site ~ .) +
#   theme_classic()
# 
# #rolling mean (using zoo package)
# temperature_rollmean = temperature_mean %>% #calculate a rolling mean using the temperature_mean dataframe
#   mutate(mean.30 = rollmean(x = temp.degC  , 30, align = "right", fill = NA)) #calculate rolling mean over a window of 30 days on variable temp.degC
# tail(temperature_rollmean)
# 
# 
# # Plot rolling function temperature data - 30 days
# temperature_rollmean %>% filter(measure == "Avg") %>%
#   ggplot(aes(x = dateTime, y = mean.30, col = site)) +
#   geom_line() +
#   facet_grid(site ~ .) +
#   theme_classic()
# 
# YLab_temp <- expression("Rolling Mean 30d Temperature" ~ (C))
# XLab_date <- expression("Date" ~ (Year-Month))
# 
# temperature_rollmean %>% filter(measure == "Avg") %>%
#   ggplot(aes(x = dateTime, y = mean.30, col = site)) +
#   xlab(XLab_date) +
#   ylab(YLab_temp) +
#   geom_line() +
#   ylim(5, 14)+
#   #geom_point() +
#   #geom_smooth(method="loess") + #likely need polynomial fit here
#   #facet_grid(site ~ .) +
#   scale_colour_discrete(name="Site",
#                         labels=c("Choked Pass", "Pruth Bay"))+
#   theme_classic()
# 
