
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
ds.rPas.activity <- readRDS(paste0(dir.providedData, 
                                   "dt.rotterdamPas.RData"))
dt.rPas.activity <- as.data.table(ds.rPas.activity)

# Summarize the number of users per day per activity
dt.rPas.activity <- dt.rPas.activity[, freq_per_year := .N, by = c("activity_nb", "year")]

# Sort dataset by postcode
dt.rPas.activity <- dt.rPas.activity[order(dt.rPas.activity$partner_postcode), ]

df.rPas.activity <- as.data.frame(dt.rPas.activity)

# load and prepare geo tag dataset
ds.zipcodes.geoloc <- read.csv(paste0(dir.providedData, 
                                      "Postalcodes_with_GeoLoc.csv"))
dt.zipcodes.geoloc <- as.data.table(ds.zipcodes.geoloc)
colnames(dt.zipcodes.geoloc)[2] <- "partner_postcode"

#merge the two datasets
df.rPas.activity <- merge(df.rPas.activity, 
                          dt.zipcodes.geoloc, 
                          by = "partner_postcode", 
                          all.x = TRUE)

# Deleting duplicates
df.rPas.activity <- df.rPas.activity[!duplicated(df.rPas.activity[c("activity_nb", 
                                                                    "year")]), ]


# Selectiung relevant columns for further analysis / map
df.rPas.activity <- df.rPas.activity[, c(2, 13, 27, 30, 33, 34)]

dt.rPas.activity <- as.data.table(df.rPas.activity)


dt.rPas.activity <- dt.rPas.activity[!is.na(dt.rPas.activity$location.lat), ]
dt.rPas.activity <- dt.rPas.activity[!is.na(dt.rPas.activity$location.lng), ]

# Summarize by Postcode/ geocode
dt.rPas.activity.pc <- dt.rPas.activity[, freq_per_year_pc := sum(freq_per_year), by = c("year", "location.lat", "location.lng")]

# Deleting duplicates
dt.rPas.activity.pc <- dt.rPas.activity.pc[, c("year", "location.lat", "location.lng", "freq_per_year_pc")]
dt.rPas.activity.pc <- dt.rPas.activity.pc[!duplicated(dt.rPas.activity.pc), ]

# Plot frequency distribution
dt.rPas.activity.pc <- dt.rPas.activity.pc[order(freq_per_year_pc), ]

View(dt.rPas.activity.pc)

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


#### Create map for 2017 ####

map.activity.pc.2017 <- ggmap(map.rotterdam.02)
map.activity.pc.2017

# Add the activity information from the RotterdamPas dataset
map.activity.pc.2017 <- map.activity.pc.2017 + 
  geom_point(aes(x = location.lng, 
                 y = location.lat, 
                 colour = cut(freq_per_year_pc,
                              c(0, 50, 500, 2000, 5000, 15000, 40000, Inf),
                              labels = c("<= 50", "51 - 500", "501 - 2,000",
                                         "2,001 - 5,000", "5,001 - 15,000",
                                         "15,001 - 40,000", "> 40,000"))),
             data = dt.rPas.activity.pc[year == 2017, ], 
             size = 1.5, 
  ) + 
  scale_colour_brewer(palette = "YlOrRd") + 
  xlab(label = "Longitude") + 
  ylab(label = "Latitude") +
  labs(title = "Activities Used by RotterdamPas Owners", 
       subtitle = "2017", 
       colour = "Number of Users") + 
  theme(plot.title = element_text(hjust = 0.5, color = "#666666"),
        plot.subtitle = element_text(hjust = 0.5, color = "#666666"))

map.activity.pc.2017

ggsave(paste0(dir.results, "map.activity.pc.2017.pdf"))



#### Create map for 2018 ####

map.activity.pc.2018 <- ggmap(map.rotterdam.02)
map.activity.pc.2018

# Add the activity information from the RotterdamPas dataset
map.activity.pc.2018 <- map.activity.pc.2018 + 
  geom_point(aes(x = location.lng, 
                 y = location.lat, 
                 colour = cut(freq_per_year_pc,
                              c(0, 50, 500, 2000, 5000, 15000, 40000, Inf),
                              labels = c("<= 50", "51 - 500", "501 - 2,000",
                                         "2,001 - 5,000", "5,001 - 15,000",
                                         "15,001 - 40,000", "> 40,000"))),
             data = dt.rPas.activity.pc[year == 2018, ], 
             size = 1.5, 
  ) + 
  scale_colour_brewer(palette = "YlOrRd") + 
  xlab(label = "Longitude") + 
  ylab(label = "Latitude") +
  labs(title = "Activities Used by RotterdamPas Owners", 
       subtitle = "2018", 
       colour = "Number of Users") + 
  theme(plot.title = element_text(hjust = 0.5, color = "#666666"),
        plot.subtitle = element_text(hjust = 0.5, color = "#666666"))

map.activity.pc.2018

ggsave(paste0(dir.results, "map.activity.pc.2018.pdf"))
