# install and load necessary libraries

# install.packages("data.table", dependencies = TRUE)
library(data.table)

# install.packages("ggmap", dependencies = TRUE)
library(ggmap)

# load rotterdam pas data
ds.rotterdamPas <- readRDS(paste0(dir.providedData, 
                                  "ds.rotterdamPas.RData"))
df.rotterdamPas.map <- as.data.frame(ds.rotterdamPas)

# select only specific location related partner columns
df.rotterdamPas.map <- df.rotterdamPas.map[, c(1, 8, 9, 10, 11, 12)]
rm(ds.rotterdamPas)

# create limited dataset for test purposes
df.rotterdamPas.map.test <- df.rotterdamPas.map[1:1000, ]
rm(df.rotterdamPas.map)

# Sort dataset by postcode
df.rotterdamPas.map.test <- df.rotterdamPas.map.test[order(df.rotterdamPas.map.test$partner_postcode), ]

# load and prepare geo tag dataset
ds.zipcodes.geoloc <- read.csv(paste0(dir.providedData, 
                                      "Postalcodes_with_GeoLoc.csv"))
df.zipcodes.geoloc <- as.data.frame(ds.zipcodes.geoloc)
rm(ds.zipcodes.geoloc)
dt.zipcodes.geoloc <- as.data.table(df.zipcodes.geoloc)
rm(df.zipcodes.geoloc)
colnames(dt.zipcodes.geoloc)[2] <- "partner_postcode_cl"

# create new column that prepares postcode in right format
dt.rotterdamPas.map.test <- as.data.table(df.rotterdamPas.map.test)
rm(df.rotterdamPas.map.test)
dt.rotterdamPas.map.test <- 
  dt.rotterdamPas.map.test[, "partner_postcode_cl" := gsub(" ", 
                                                           "", 
                                                           dt.rotterdamPas.map.test$partner_postcode)]

#merge the two datasets
dt.rPas.partner <- merge(dt.rotterdamPas.map.test, 
                         dt.zipcodes.geoloc, 
                         by = "partner_postcode_cl", 
                         all.x = TRUE
                         )

dt.rPas.partner[, c("partner_postcode_cl", 
                    "partner_postcode", 
                    "partner_p4", 
                    "X", 
                    "formal.address")] <- NULL

View(dt.rPas.partner)

str(dt.rPas.partner)

##### Defining underlining map of Rotterdam #####

# Determine how large the map should be (bounding box) given the
# geocoordinates to plot
minLat <- min(dt.rPas.partner$location.lat, na.rm = TRUE)
maxLat <- max(dt.rPas.partner$location.lat, na.rm = TRUE)
minLon <- min(dt.rPas.partner$location.lng, na.rm = TRUE)
maxLon <- max(dt.rPas.partner$location.lng, na.rm = TRUE)

rangeLat <- maxLat - minLat
rangeLon <- maxLon - minLon

mrg  <- 0.15   # Apply 15% margin in all directions
bbox <- c(minLon - mrg*rangeLon, minLat - mrg*rangeLat, 
          maxLon + mrg*rangeLon, maxLat + mrg*rangeLat)

# Get the map
map.rotterdam.01 <- get_stamenmap(bbox, 
                               zoom = 12, 
                               maptype = "terrain")

# Store the map
save(map.rotterdam.01, file = paste0(dir.results, 
                                     "map.rotterdam.01.Rda"))

# Show the map
map.Activity.01 <- ggmap(map.rotterdam.01)

# Add the Parking garages information. The plotting of 
# both colour and shape is redundant in this case, as 
# these aesthetics contain the same information
map.Activity.01 + 
  geom_point(aes(x = location.lng, 
                 y = location.lat 
                 # colour = TYPE, 
                 # shape=TYPE
                 ), 
             data = dt.rPas.partner, 
             size = 5
             )
  # scale_color_brewer("Type of garage", palette = "Set1") +
  # scale_shape_discrete(guide=FALSE)
map.Activity.01
ggsave(paste0(dir.results,"map.Activity.01.pdf"))
