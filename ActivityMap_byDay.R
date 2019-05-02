##### install and load necessary libraries #####

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


##### Import and prepare data #####

# load rotterdam pas data
ds.rPas.activity <- readRDS(paste0(dir.providedData, 
                                   "dt.rotterdamPas.RData"))
dt.rPas.activity <- as.data.table(ds.rPas.activity)

# Summarize the number of users per day per activity
dt.rPas.activity <- dt.rPas.activity[, freq_per_day := .N, by = c("activity_nb", "use_date")]

# Summarize the number of users per activity
dt.rPas.activity <- dt.rPas.activity[, freq_per_activity := .N, by = c("activity_nb")]

# Sort dataset by postcode
dt.rPas.activity.test <- dt.rPas.activity.test[order(dt.rPas.activity.test$partner_postcode), ]

df.rPas.activity.test <- as.data.frame(dt.rPas.activity.test)

# load and prepare geo tag dataset
ds.zipcodes.geoloc <- read.csv(paste0(dir.providedData, 
                                      "Postalcodes_with_GeoLoc.csv"))
dt.zipcodes.geoloc <- as.data.table(ds.zipcodes.geoloc)
colnames(dt.zipcodes.geoloc)[2] <- "partner_postcode"

#merge the two datasets
df.rPas.activity.test <- merge(df.rPas.activity.test, 
                               dt.zipcodes.geoloc, 
                               by = "partner_postcode", 
                               all.x = TRUE)


# Deleting duplicates
df.rPas.activity.test.freqActivity <- df.rPas.activity.test[firstobs(df.rPas.activity.test$activity_nb), ]

df.rPas.activity.test.freqDay <- df.rPas.activity.test[!duplicated(df.rPas.activity.test[c("activity_nb", 
                                                                                           "use_date")]), ]
# Selectiung relevant columns for further analysis / map
df.rPas.activity.test.freqDay <- df.rPas.activity.test[, c(2, 13, 22, 29, 33, 34)]

df.rPas.activity.test.freqActivity <- df.rPas.activity.test.freqActivity[, c(2, 13, 22, 27, 
                                                                             30, 33, 34)]

dt.rPas.activity.test.freqActivity <- as.data.table(df.rPas.activity.test.freqActivity)




##### Defining underlining map of Rotterdam #####

# Determine how large the map should be (bounding box) given the geocoordinates to plot
minLat <- min(df.rPas.activity.test.freqActivity$location.lat, na.rm = TRUE)
maxLat <- max(df.rPas.activity.test.freqActivity$location.lat, na.rm = TRUE)
minLon <- min(df.rPas.activity.test.freqActivity$location.lng, na.rm = TRUE)
maxLon <- max(df.rPas.activity.test.freqActivity$location.lng, na.rm = TRUE)

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
map.rotterdam.01 <- ggmap(map.rotterdam.01)
map.rotterdam.01

# Add the activity information from the RotterdamPas dataset for 2017
map.Activity.2017 <- map.rotterdam.01 + 
  geom_point(aes(x = location.lng, 
                 y = location.lat,
                 colour = cut(freq_per_activity, 
                              c(0, 10, 100, 500, 2000, 10000, 30000, Inf), 
                              labels = c("<= 10", "11 - 100", "101 - 500", 
                                         "501 - 2,000", "2,001 - 10,000", 
                                         "10,001 - 30,000", "> 30,000"))), 
             data = dt.rPas.activity.test.freqActivity[year == 2017, ], 
             size = 1
  ) + 
  scale_colour_brewer(palette = "YlOrRd") +
  ggtitle(label = "Activities Used by RotterdamPas Owners in 2017") +
  xlab(label = "Longitude") + 
  ylab(label = "Latitude") + 
  labs(colour = "Number of Users")

ggsave(paste0(dir.results,"map.Activity.2017.pdf"))


# Add the activity information from the RotterdamPas dataset for 2018
map.Activity.2018 <- map.rotterdam.01 + 
  geom_point(aes(x = location.lng, 
                 y = location.lat,
                 colour = cut(freq_per_activity, 
                              c(0, 10, 100, 500, 2000, 10000, 30000, Inf), 
                              labels = c("<= 10", "11 - 100", "101 - 500", 
                                         "501 - 2,000", "2,001 - 10,000", 
                                         "10,001 - 30,000", "> 30,000"))), 
             data = dt.rPas.activity.test.freqActivity[year == 2018, ], 
             size = 1
  ) + 
  scale_colour_brewer(palette = "YlOrRd") +
  ggtitle(label = "Activities Used by RotterdamPas Owners in 2018") +
  xlab(label = "Longitude") + 
  ylab(label = "Latitude") + 
  labs(colour = "Number of Users")

ggsave(paste0(dir.results,"map.Activity.2018.pdf"))