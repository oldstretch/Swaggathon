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
ds.rPas.activity.month <- readRDS(paste0(dir.providedData, 
                                         "dt.rotterdamPas.RData"))
dt.rPas.activity.month <- as.data.table(ds.rPas.activity.month)

dt.rPas.activity.month <- dt.rPas.activity.month[, month_of_year := months(dt.rPas.activity.month$use_date)]

# Summarize the number of users per day per activity
dt.rPas.activity.month <- dt.rPas.activity.month[, freq_per_month := .N, by = c("activity_nb", "month_of_year")]

# Sort dataset by postcode
dt.rPas.activity.month <- dt.rPas.activity.month[order(dt.rPas.activity.month$partner_postcode), ]

df.rPas.activity.month <- as.data.frame(dt.rPas.activity.month)


# load and prepare geo tag dataset
ds.zipcodes.geoloc <- read.csv(paste0(dir.providedData, 
                                      "Postalcodes_with_GeoLoc.csv"))
dt.zipcodes.geoloc <- as.data.table(ds.zipcodes.geoloc)
colnames(dt.zipcodes.geoloc)[2] <- "partner_postcode"

#merge the two datasets
df.rPas.activity.month <- merge(df.rPas.activity.month, 
                                dt.zipcodes.geoloc, 
                                by = "partner_postcode", 
                                all.x = TRUE)

# Deleting duplicates
df.rPas.activity.month <- df.rPas.activity.month[!duplicated(df.rPas.activity.month[c("activity_nb", 
                                                                                      "month_of_year")]), ]
# Selectiung relevant columns for further analysis / map
df.rPas.activity.month <- df.rPas.activity.month[, c(2, 13, 30, 31, 34, 35)]

dt.rPas.activity.month <- as.data.table(df.rPas.activity.month)

dt.rPas.activity.month <- dt.rPas.activity.month[!is.na(location.lat), ]

dt.rPas.activity.month$month_of_year <- factor(dt.rPas.activity.month$month_of_year, 
                                               levels = c("Januar", "Februar", "März", "April", "Mai", 
                                                          "Juni", "Juli", "August", "September", 
                                                          "Oktober", "November", "Dezember"), 
                                               labels = c("January", "February", "March", "April", "May", 
                                                          "June", "Juli", "August", "September", 
                                                          "October", "November", "December"))



##### Defining underlining map of Rotterdam #####

# # Determine how large the map should be (bounding box) given the geocoordinates to plot
# minLat <- min(dt.rPas.activity.month$location.lat, na.rm = TRUE)
# maxLat <- max(dt.rPas.activity.month$location.lat, na.rm = TRUE)
# minLon <- min(dt.rPas.activity.month$location.lng, na.rm = TRUE)
# maxLon <- max(dt.rPas.activity.month$location.lng, na.rm = TRUE)
# 
# rangeLat <- maxLat - minLat
# rangeLon <- maxLon - minLon
# 
# mrg  <- 0.10   # Apply 10% margin in all directions
# bbox <- c(minLon - mrg*rangeLon, minLat - mrg*rangeLat, 
#           maxLon + mrg*rangeLon, maxLat + mrg*rangeLat)
# 
# # Get the map
# map.rotterdam.03 <- get_stamenmap(bbox, 
#                                   zoom = 12, 
#                                   maptype = "terrain")
# 
# # Store the map
# save(map.rotterdam.03, file = paste0(dir.results, "map.rotterdam.03.Rda"))

load(paste0(dir.results, "map.rotterdam.02.Rda"))


# Show the map
map.activity.months <- ggmap(map.rotterdam.02)
map.activity.months

# Add the activity information from the RotterdamPas dataset for 2017
map.activity.months <- map.activity.months + 
  geom_point(aes(x = location.lng, 
                 y = location.lat,
                 colour = cut(freq_per_month, 
                              c(0, 10, 50, 100, 250, 1000, 5000, Inf), 
                              labels = c("<= 10", "11 - 50", "51 - 100", 
                                         "101 - 250", "251 - 1,000", 
                                         "1,001 - 5,000", "> 5,001"))), 
             data = dt.rPas.activity.month, 
             size = 1.5,
             alpha = 0.50
  ) + 
  scale_colour_brewer(palette = "YlOrRd") +
  xlab(label = "Longitude") + 
  ylab(label = "Latitude") + 
  labs(colour = "Number of Users")

map.activity.months

map.activity.months.animated <- map.activity.months + 
  transition_states(dt.rPas.activity.month$month_of_year, 
                    transition_length = 1, 
                    state_length = 25) + 
  labs(title = "Activities Used by RotterdamPas Owners per Month", 
       subtitle = "{closest_state}")


gganimate::animate(map.activity.months.animated, renderer = av_renderer())

anim_save(paste0(dir.results, "map.activity.months.animated.mp4"))


##### Static map that shows activities during Juli #####

# Limit dataset to activities that take place during Juli

dt.rPas.activity.month.juli <- dt.rPas.activity.month[month_of_year == "Juli", ]


# Create graph that only consist of activities on Juli
map.activity.month.juli <- map.activity.months + 
  geom_point(aes(x = location.lng, 
                 y = location.lat, 
                 colour = cut(freq_per_month, 
                              c(0, 10, 50, 100, 250, 1000, 5000, Inf), 
                              labels = c("<= 10", "11 - 50", "51 - 100", 
                                         "101 - 250", "251 - 1,000", 
                                         "1,001 - 5,000", "> 5,001"))), 
             data = dt.rPas.activity.month.wednesday, 
             size = 1.5, 
             alpha = 0.50
  ) + 
  scale_colour_brewer(palette = "YlOrRd") + 
  xlab(label = "Longitude") +
  ylab(label = "Latitude") +
  labs(title = "Activities Used by RotterdamPas Owners per Month", 
       subtitle = "Juli", 
       colour = "Number of Users")

map.activity.month.juli
ggsave(paste0(dir.results, "map.activity.month.juli.pdf"))



##### Static map that shows activities during December #####

# Limit dataset to activities that take place during December

dt.rPas.activity.month.december <- dt.rPas.activity.month[month_of_year == "December", ]


# Create graph that only consist of activities on December
map.activity.month.december <- map.activity.months + 
  geom_point(aes(x = location.lng, 
                 y = location.lat, 
                 colour = cut(freq_per_month, 
                              c(0, 10, 50, 100, 250, 1000, 5000, Inf), 
                              labels = c("<= 10", "11 - 50", "51 - 100", 
                                         "101 - 250", "251 - 1,000", 
                                         "1,001 - 5,000", "> 5,001"))), 
             data = dt.rPas.activity.month.december, 
             size = 1.5, 
             alpha = 0.50
  ) + 
  scale_colour_brewer(palette = "YlOrRd") + 
  xlab(label = "Longitude") +
  ylab(label = "Latitude") +
  labs(title = "Activities Used by RotterdamPas Owners per Month", 
       subtitle = "December", 
       colour = "Number of Users")

map.activity.month.december
ggsave(paste0(dir.results, "map.activity.month.december.pdf"))
