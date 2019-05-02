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

# install.packages("gganimate", dependencies = TRUE)
library(gganimate)


##### Import and prepare data #####

# load rotterdam pas data
ds.rPas.activity.day <- readRDS(paste0(dir.providedData, 
                                   "dt.rotterdamPas.RData"))
dt.rPas.activity.day <- as.data.table(ds.rPas.activity.day)

dt.rPas.activity.day <- dt.rPas.activity.day[!is.na(dt.rPas.activity.day$use_date), ]

dt.rPas.activity.day <- dt.rPas.activity.day[, day_of_week := weekdays(dt.rPas.activity.day$use_date)]

# Summarize the number of users per day per activity
dt.rPas.activity.day <- dt.rPas.activity.day[, freq_per_day := .N, by = c("activity_nb", "day_of_week")]

# Sort dataset by postcode
dt.rPas.activity.day <- dt.rPas.activity.day[order(dt.rPas.activity.day$partner_postcode), ]

df.rPas.activity.day <- as.data.frame(dt.rPas.activity.day)

# load and prepare geo tag dataset
ds.zipcodes.geoloc <- read.csv(paste0(dir.providedData,
                                       "Postalcodes_with_GeoLoc.csv"))
dt.zipcodes.geoloc <- as.data.table(ds.zipcodes.geoloc)
colnames(dt.zipcodes.geoloc)[2] <- "partner_postcode"

#merge the two datasets
df.rPas.activity.day <- merge(df.rPas.activity.day,
                                dt.zipcodes.geoloc,
                               by = "partner_postcode",
                               all.x = TRUE)

# Deleting duplicates
df.rPas.activity.day <- df.rPas.activity.day[!duplicated(df.rPas.activity.day[c("activity_nb", 
                                                                                "day_of_week")]), ]


# Selectiung relevant columns for further analysis / map
df.rPas.activity.day <- df.rPas.activity.day[, c(2, 13, 29, 30, 33, 34)]

dt.rPas.activity.day <- as.data.table(df.rPas.activity.day)

dt.rPas.activity.day$day_of_week <- factor(dt.rPas.activity.day$day_of_week, 
                                           levels = c("Montag", "Dienstag", "Mittwoch", "Donnerstag", 
                                                      "Freitag", "Samstag", "Sonntag"),
                                           labels = c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                                      "Friday", "Saturday", "Sunday"))

dt.rPas.activity.day <- dt.rPas.activity.day[!is.na(location.lat), ]




##### Defining underlining map of Rotterdam #####

# Determine how large the map should be (bounding box) given the geocoordinates to plot
minLat <- min(dt.rPas.activity.day$location.lat, na.rm = TRUE)
maxLat <- max(dt.rPas.activity.day$location.lat, na.rm = TRUE)
minLon <- min(dt.rPas.activity.day$location.lng, na.rm = TRUE)
maxLon <- max(dt.rPas.activity.day$location.lng, na.rm = TRUE)

rangeLat <- maxLat - minLat
rangeLon <- maxLon - minLon


mrg  <- 0.10   # Apply 15% margin in all directions
bbox <- c(minLon - mrg*rangeLon, minLat - mrg*rangeLat, 
          maxLon + mrg*rangeLon, maxLat + mrg*rangeLat)

# Get the map
map.rotterdam.02 <- get_stamenmap(bbox, 
                                  zoom = 12, 
                                  maptype = "terrain")

# Store the map
save(map.rotterdam.02, file = paste0(dir.results, "map.rotterdam.02.Rda"))


# Show the map
map.activity.days <- ggmap(map.rotterdam.02)
map.activity.days

# Add the activity information from the RotterdamPas dataset for 2017
# map.activity.days + 
#   geom_point(aes(x = location.lng, 
#                  y = location.lat,
#                  colour = day_of_week), 
#              data = dt.rPas.activity.day, 
#              size = 3,
#              alpha = 0.20
#   ) + 
#   scale_colour_brewer(palette = "YlOrRd") +
#   ggtitle(label = "Activities Used by RotterdamPas Owners per Weekday") +
#   xlab(label = "Longitude") + 
#   ylab(label = "Latitude") + 
#   labs(colour = "Number of Users")
# 
# map.activity.days + 
#   transition_states(dt.rPas.activity.day$day_of_week)

map.activity.days.animated <- map.activity.days + 
  geom_point(aes(x = location.lng, 
                 y = location.lat, 
                 colour = cut(freq_per_day,
                              c(0, 10, 50, 100, 500, 2000, 10000, Inf),
                              labels = c("<= 10", "11 - 50", "51 - 100",
                                         "101 - 500", "501 - 2,000",
                                         "2,001 - 10,000", "> 10,000"))),
             data = dt.rPas.activity.day, 
             size = 3, 
             alpha = 0.20
             ) + 
  scale_colour_brewer(palette = "YlOrRd") + 
    xlab(label = "Longitude") +
    ylab(label = "Latitude") +
    labs(colour = "Number of Users")

map.activity.days.animated

map.activity.days.animated + 
  transition_states(dt.rPas.activity.day$day_of_week, 
                    transition_length = 10, 
                    state_length = 25) + 
  labs(title = "Activities Used by RotterdamPas Owners per Weekday: ", 
       subtitle = "{closest_state}")
  

  
anim_save(paste0(dir.results, "map.activity.days.animated.mp4"))
