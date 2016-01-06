#PA Shootings Script

#packages----
library(dplyr)
library(ggplot2)
library(lubridate)
library(ggthemes)
library(RCurl)
library(rvest)
library(extrafont)
library(ggthemes)
library(grid)

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

#ggplot theme----

#Import and list fonts
loadfonts(device="win")

#Fonts to plug into font.type variable
"Lucida Sans"
"Gil Sans MT"
"Verdana"
"Trebuchet MS"
"Georgia"
"Garamond"

#Global theme options - to easily all plots at once
font.type <- "Garamond"
background.color <- "#f1f1f1"
line.color <- "#d8d8d8"
title.color <- "#3C3C3C"
title.size <- 22
axis.color <- "#535353"
axis.size <- 14

transparency <- .7 #for alpha
line.size <- 1.6 #for geom_line()
point.size <- 3 #for geom_point()

#theme
theme_bg <-theme(panel.background=element_rect(fill=background.color)) + 
  theme(plot.background=element_rect(fill=background.color)) +
  theme(panel.grid.major=element_line(colour=line.color,size=.60)) +
  theme(panel.grid.minor=element_line(colour=line.color,size=.05)) +
  theme(axis.ticks=element_blank()) +
  theme(plot.title=element_text(face="bold",vjust=2, hjust=-.07, colour=title.color,size=title.size)) +
  theme(axis.text.x=element_text(size=axis.size,colour=axis.color)) +
  theme(axis.text.y=element_text(size=axis.size,colour=axis.color)) +
  theme(axis.title.y=element_text(size=axis.size,colour=axis.color,vjust=1.5)) +
  theme(axis.title.x=element_text(size=axis.size,colour=axis.color,vjust=-.5)) +
  theme(text=element_text(family=font.type))

#theme options (to add plots inividually)

#to add bold line at y=0
geom_hline(yintercept=0,size=1.2,colour="#535353")

#to change plot margins
theme(plot.margin = unit(c(1, 1, .5, .7), "cm"))

#to get rid of legend
theme(legend.position="none")
guides(fill = FALSE)

#to format legend when it's needed
theme(legend.background = element_rect(fill=background.color)) + 
  theme(legend.key = element_rect(colour = background.color)) + 
  theme(legend.direction = "horizontal", legend.position = "bottom")


#exploration and plots----

sum(data$count) #total shootings
data %>% group_by(state) %>% summarise(total = sum(count)) %>% ungroup() %>% arrange(desc(total)) #top 10 states
data %>% group_by(city) %>% summarise(total = sum(count)) %>% ungroup() %>% arrange(desc(total)) #top 10 cities
data %>% group_by(race) %>% summarise(total = sum(count)) %>% ungroup() %>% arrange(desc(total)) #by race

pa <- data %>% filter(state == "PA")
