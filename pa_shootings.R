#PA Shootings Script

#packages----
library(dplyr)
library(ggplot2)
library(lubridate)
library(ggthemes)
library(RCurl)
library(rvest)

#loads and formats data----
data <- read.csv("https://raw.githubusercontent.com/washingtonpost/data-police-shootings/master/fatal-police-shootings-data.csv")
data$date <- ymd(data$date)
data$age <- as.numeric(data$age)
data$state.flag <- "other"
data$state.flag[data$state == "PA"] <- "PA"
data$state.flag <- as.factor(data$state.flag)
data$city.flag <- "other"
data$city.flag[data$city== "Philadelphia"] <- "PHL"
data$city.flag <- as.factor(data$city.flag)
data$month <- month(data$date, label = TRUE)
data$year <- year(data$date)
data$year <- as.factor(data$year)
data$day <- wday(data$date, label = TRUE, abbr = TRUE)

#check data structure
str(data)
