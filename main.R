# install and import the necessary packages

# install.packages("knitr", dependencies = TRUE)
library(knitr)


# set working directory (individual per teammember!!!)

# Elena's working directory
setwd("C:/Users/elena/OneDrive/Desktop/Big Data and Business Analytics/Swaggathon/providedData")

# Edwin's working directory
# setwd("XXX")

# Lisa's working directory
# setwd("XXX")

# Felix's working directory
setwd("/Users/felixmeindl/Documents/GitHub/")

# Uli's working directory
# setwd("XXX")

path.providedData <- "/Swaggathon/providedData"
path.additionalData <- "/Swaggathon/additionalData"
path.results <- "/Swaggathon/results"


# set global seed
set.seed(1234)


# run all hackathon scripts
source("data_cleaning.R")


# save script as pdf
knitr::stitch('main.R')


dt.weather <- read.delim("ds.weather.txt")
dt.weather <- as.data.frame(dt.weather)
library(stringr)
names(dt.weather) <- c("wt")

dt.weather <- str_split_fixed(dt.weather$wt, ",", 12)
dt.weather <- dt.weather[19:1115, 2:12]
dt.weather <- dt.weather[, c(-3,-5,-6,-8,-10)]
dt.weather <- as.data.frame(dt.weather)
names(dt.weather) <- c("Date", 
                       "Daily Avg. Wind Speed", 
                       "Daily Avg. Temperature", 
                       "Sunshine Duration", 
                       "Prec. Duration",
                       "Highest h. amount prec.")
dt.weather <- dt.weather[3:1097, ]
dt.weather$Date <- as.character(dt.weather$Date)
dt.weather$Date <- sub("([[:digit:]]{4,4})$", "/\\1", dt.weather$Date)

dt.weather$Date <- as.Date(dt.weather$Date)


