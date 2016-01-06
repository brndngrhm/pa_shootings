#PA Shootings Script

#packages----
library(dplyr)
library(ggplot2)
library(lubridate)
library(ggthemes)
library(RCurl)
library(rvest)

#loads and formats data----

#shooting data
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
data$count <- 1

#loads and checks structure
View(data)
str(data)

#state population data
states <- read.csv("http://www.census.gov/popest/data/state/totals/2015/tables/NST-EST2015-01.csv", header = FALSE)
states <- states[-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 61, 62, 63, 64, 65, 66, 67), -c(2,3,4,5,6,7,8)] #removes unwanted rows and columns, keeps only 2015 population and state name
names(states)[1] <- "state"
names(states)[2] <- "pop"
states$state <- sub('.', '', states$state) #removes "." at beginning of state names
states$pop <- gsub(',', '', states$pop) #removes commas from population column
states$state <- state.abb[match(states$state,state.name)] #adds state abbreviations
states[9,1] <- "DC" #Adds Washington DC abbreviation
states$state <- as.factor(states$state)
states$pop <- as.numeric(states$pop)

#loads and checks structure
View(states)
str(states)

data <- left_join(data, states, by = "state")
data$state <- as.factor(data$state)
str(data)


#exploration and plots----
(state <- ggplot(data, aes(x=reorder(state, count), y=count, fill = state.flag)) + geom_bar(stat = "identity"))
