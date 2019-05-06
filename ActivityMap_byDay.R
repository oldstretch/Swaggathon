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


# # save script as pdf
# knitr::stitch('ActivityMap_byDay.R')


##### Import and prepare data #####

# load rotterdam pas data
ds.rPas.activity.day <- readRDS(paste0(dir.providedData, 
                                   "dt.rotterdamPas.RData"))
dt.rPas.activity.day <- as.data.table(ds.rPas.activity.day)

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
df.rPas.activity.day <- df.rPas.activity.day[, c(2, 13, 30, 31, 34, 35)]

dt.rPas.activity.day <- as.data.table(df.rPas.activity.day)

dt.rPas.activity.day$day_of_week <- factor(dt.rPas.activity.day$day_of_week, 
                                           levels = c("Montag", "Dienstag", "Mittwoch", "Donnerstag", 
                                                      "Freitag", "Samstag", "Sonntag"), 
                                           labels = c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                                      "Friday", "Saturday", "Sunday"))

dt.rPas.activity.day <- dt.rPas.activity.day[!is.na(dt.rPas.activity.day$day_of_week), ]
dt.rPas.activity.day <- dt.rPas.activity.day[!is.na(dt.rPas.activity.day$location.lat), ]
dt.rPas.activity.day <- dt.rPas.activity.day[!is.na(dt.rPas.activity.day$location.lng), ]

# Summarize by Postcode/ geocode
dt.rPas.activity.day.pc <- dt.rPas.activity.day[, freq_per_day_pc := sum(freq_per_day), by = c("day_of_week", "location.lat", "location.lng")]

# Deleting duplicates
dt.rPas.activity.day.pc <- dt.rPas.activity.day.pc[, c("day_of_week", "location.lat", "location.lng", "freq_per_day_pc")]
dt.rPas.activity.day.pc <- dt.rPas.activity.day.pc[!duplicated(dt.rPas.activity.day.pc), ]

# Plot frequency distribution
dt.rPas.activity.day.pc <- dt.rPas.activity.day.pc[order(freq_per_day_pc), ]

##### Defining underlining map of Rotterdam #####

# # Determine how large the map should be (bounding box) given the geocoordinates to plot 
# minLat <- min(dt.rPas.activity.day$location.lat, na.rm = TRUE)
# maxLat <- max(dt.rPas.activity.day$location.lat, na.rm = TRUE)
# minLon <- min(dt.rPas.activity.day$location.lng, na.rm = TRUE)
# maxLon <- max(dt.rPas.activity.day$location.lng, na.rm = TRUE)
# 
# rangeLat <- maxLat - minLat
# rangeLon <- maxLon - minLon
# 
# mrg  <- 0.10   # Apply 10% margin in all directions
# bbox <- c(minLon - mrg*rangeLon, minLat - mrg*rangeLat, 
#           maxLon + mrg*rangeLon, maxLat + mrg*rangeLat)
# 
# # Get the map
# map.rotterdam.02 <- get_stamenmap(bbox, 
#                                   zoom = 12, 
#                                   maptype = "terrain")

# # Store the map
# save(map.rotterdam.02, file = paste0(dir.results, "map.rotterdam.02.Rda"))

# Load saved map of Rotterdam 
load(paste0(dir.results, "map.rotterdam.02.Rda"))

# Show the map
map.activity.days.pc <- ggmap(map.rotterdam.02)
map.activity.days.pc

# Add the activity information from the RotterdamPas dataset
map.activity.days.pc <- map.activity.days.pc + 
  geom_point(aes(x = location.lng, 
                 y = location.lat, 
                 colour = cut(freq_per_day_pc,
                              c(0, 10, 100, 500, 2000, 5000, 15000, Inf),
                              labels = c("<= 10", "11 - 100", "101 - 500",
                                         "501 - 2,000", "2,001 - 5,000",
                                         "5,001 - 15,000", "> 15,000"))),
             data = dt.rPas.activity.day.pc, 
             size = 1.5, 
             ) + 
  scale_colour_brewer(palette = "YlOrRd") + 
    xlab(label = "Longitude") +
    ylab(label = "Latitude") +
    labs(colour = "Number of Users")

map.activity.days.pc

map.activity.days.pc.animated <- map.activity.days.pc + 
  transition_states(dt.rPas.activity.day.pc$day_of_week, 
                    transition_length = 1, 
                    state_length = 25) + 
  labs(title = "Activities Used by RotterdamPas Owners per Day", 
       subtitle = "{closest_state}") + 
  theme(plot.title = element_text(hjust = 0.5, color = "#666666"),
        plot.subtitle = element_text(hjust = 0.5, color = "#666666"))
  
gganimate::animate(map.activity.days.pc.animated, renderer = av_renderer())

anim_save(paste0(dir.results, "map.activity.days.pc.animated.mp4"))


##### Static map that shows activities on Wednesdays #####

# Limit dataset to activities that take place on Wednesdays 

dt.rPas.activity.day.pc.wednesday <- dt.rPas.activity.day.pc[day_of_week == "Wednesday"]


# Create graph that only consist of activities on Wednesdays
map.activity.days.pc.wednesday <- ggmap(map.rotterdam.02)
map.activity.days.pc.wednesday

map.activity.days.pc.wednesday <- map.activity.days.pc.wednesday + 
  geom_point(aes(x = location.lng, 
                 y = location.lat, 
                 colour = cut(freq_per_day_pc,
                              c(0, 10, 100, 500, 2000, 5000, 15000, Inf),
                              labels = c("<= 10", "11 - 100", "101 - 500",
                                         "501 - 2,000", "2,001 - 5,000",
                                         "5,001 - 15,000", "> 15,000"))),
             data = dt.rPas.activity.day.pc.wednesday, 
             size = 1.5 
  ) + 
  scale_colour_brewer(palette = "YlOrRd") + 
  xlab(label = "Longitude") +
  ylab(label = "Latitude") +
  labs(title = "Activities Used by RotterdamPas Owners per Day", 
       subtitle = "Wednesday", 
       colour = "Number of Users") + 
  theme(plot.title = element_text(hjust = 0.5, color = "#666666"),
        plot.subtitle = element_text(hjust = 0.5, color = "#666666"))

map.activity.days.pc.wednesday
ggsave(paste0(dir.results, "map.activity.days.pc.wednesday.pdf"))



##### Static map that shows activities on Sundays #####

# Limit dataset to activities that take place on Sundays

dt.rPas.activity.day.pc.sunday <- dt.rPas.activity.day.pc[day_of_week == "Sunday"]


# Create graph that only consist of activities on Sundays
map.activity.days.pc.sunday <- ggmap(map.rotterdam.02)
map.activity.days.pc.sunday

map.activity.days.pc.sunday <- map.activity.days.pc.sunday + 
  geom_point(aes(x = location.lng, 
                 y = location.lat, 
                 colour = cut(freq_per_day_pc,
                              c(0, 10, 100, 500, 2000, 5000, 15000, Inf),
                              labels = c("<= 10", "11 - 100", "101 - 500",
                                         "501 - 2,000", "2,001 - 5,000",
                                         "5,001 - 15,000", "> 15,000"))),
             data = dt.rPas.activity.day.pc.sunday, 
             size = 1.5 
  ) + 
  scale_colour_brewer(palette = "YlOrRd") + 
  xlab(label = "Longitude") +
  ylab(label = "Latitude") +
  labs(title = "Activities Used by RotterdamPas Owners per Day", 
       subtitle = "Sunday", 
       colour = "Number of Users") + 
  theme(plot.title = element_text(hjust = 0.5, color = "#666666"),
        plot.subtitle = element_text(hjust = 0.5, color = "#666666"))

map.activity.days.pc.sunday
ggsave(paste0(dir.results, "map.activity.days.pc.sunday.pdf"))

