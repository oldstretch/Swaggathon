# install and load necessary libraries

# install.packages("data.table", dependencies = TRUE)
library(data.table)

# install.packages("ggmap", dependencies = TRUE)
library(ggmap)

#install.packages("dplyr", dependencies = TRUE)
library(dplyr)

#install.packages("plyr", dependencies = TRUE)
library(plyr)

# install.packages("RColorBrewer", dependencies = TRUE)
library(RColorBrewer)

# install.packages("doBy", dependencies = TRUE)
library(doBy)



# load rotterdam pas data
ds.rPas.activity.yr <- readRDS(paste0(dir.providedData, 
                                  "dt.rotterdamPas.RData"))
dt.rPas.activity.yr <- as.data.table(ds.rPas.activity.yr)

# Summarize the number of users per activity
dt.rPas.activity.yr <- dt.rPas.activity.yr[, freq_per_activity := .N, by = c("activity_nb")]

# Sort dataset by postcode
dt.rPas.activity.yr <- dt.rPas.activity.yr[order(dt.rPas.activity.yr$partner_postcode), ]

df.rPas.activity.yr <- as.data.frame(dt.rPas.activity.yr)

# load and prepare geo tag dataset
ds.zipcodes.geoloc <- read.csv(paste0(dir.providedData, 
                                      "Postalcodes_with_GeoLoc.csv"))
dt.zipcodes.geoloc <- as.data.table(ds.zipcodes.geoloc)
colnames(dt.zipcodes.geoloc)[2] <- "partner_postcode"

#merge the two datasets
df.rPas.activity.yr <- merge(df.rPas.activity.yr, 
                          dt.zipcodes.geoloc, 
                          by = "partner_postcode", 
                          all.x = TRUE)

# Deleting duplicates
df.rPas.activity.yr <- df.rPas.activity.yr[firstobs(df.rPas.activity.yr$activity_nb), ]

df.rPas.activity.yr <- df.rPas.activity.yr[, c(2, 13, 27, 29, 32, 33)]

dt.rPas.activity.yr <- as.data.table(df.rPas.activity.yr)



##### Defining underlining map of Rotterdam #####

# Determine how large the map should be (bounding box) given the geocoordinates to plot
minLat <- min(dt.rPas.activity.yr$location.lat, na.rm = TRUE)
maxLat <- max(dt.rPas.activity.yr$location.lat, na.rm = TRUE)
minLon <- min(dt.rPas.activity.yr$location.lng, na.rm = TRUE)
maxLon <- max(dt.rPas.activity.yr$location.lng, na.rm = TRUE)

rangeLat <- maxLat - minLat
rangeLon <- maxLon - minLon

mrg  <- 0.10   # Apply 15% margin in all directions
bbox <- c(minLon - mrg*rangeLon, minLat - mrg*rangeLat, 
          maxLon + mrg*rangeLon, maxLat + mrg*rangeLat)

# Get the map
map.rotterdam.01 <- get_stamenmap(bbox, 
                               zoom = 12, 
                               maptype = "terrain")

# Store the map
save(map.rotterdam.01, file = paste0(dir.results, "map.rotterdam.01.Rda"))

# Show the map
map.Activity.2017 <- ggmap(map.rotterdam.01)
map.Activity.2017

# Add the activity information from the RotterdamPas dataset for 2017
map.Activity.2017 + 
  geom_point(aes(x = location.lng, 
                 y = location.lat,
                 colour = cut(freq_per_activity, 
                              c(0, 10, 100, 500, 2000, 10000, 30000, Inf), 
                              labels = c("<= 10", "11 - 100", "101 - 500", 
                                         "501 - 2,000", "2,001 - 10,000", 
                                         "10,001 - 30,000", "> 30,000"))), 
             data = dt.rPas.activity.yr[year == 2017, ], 
             size = 4,
             alpha = 0.5
             ) + 
  scale_colour_brewer(palette = "YlOrRd") +
  ggtitle(label = "Activities Used by RotterdamPas Owners in 2017") +
  xlab(label = "Longitude") + 
  ylab(label = "Latitude") + 
  labs(colour = "Number of Users")


# ggsave(paste0(dir.results,"map.Activity.2017.pdf"))


# Add the activity information from the RotterdamPas dataset for 2018
# Show the map
map.Activity.2018 <- ggmap(map.rotterdam.01)
map.Activity.2018

map.Activity.2018 + 
  geom_point(aes(x = location.lng, 
                 y = location.lat,
                 colour = cut(freq_per_activity, 
                              c(0, 10, 100, 500, 2000, 10000, 30000, Inf), 
                              labels = c("<= 10", "11 - 100", "101 - 500", 
                                         "501 - 2,000", "2,001 - 10,000", 
                                         "10,001 - 30,000", "> 30,000"))), 
  data = dt.rPas.activity.yr[year == 2018, ], 
  size = 4,
  alpha = 0.5
  ) + 
  scale_colour_brewer(palette = "YlOrRd") +
  ggtitle(label = "Activities Used by RotterdamPas Owners in 2018") +
  xlab(label = "Longitude") + 
  ylab(label = "Latitude") + 
  labs(colour = "Number of Users")

# ggsave(paste0(dir.results,"map.Activity.2018.pdf"))


# save script as pdf
knitr::stitch('ActivityMap_byDay.R')
