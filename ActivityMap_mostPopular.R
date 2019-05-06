# load rotterdam pas data
ds.rPas.activity.mostPop <- readRDS(paste0(dir.providedData, 
                                   "dt.rotterdamPas.RData"))
dt.rPas.activity.mostPop <- as.data.table(ds.rPas.activity)

# select only the most popular activities (>5% of total "transactions" as identified earlier)
dt.rPas.activity.mostPop  <- subset(dt.rPas.activity.mostPop, 
                                    activity_type == "Film" | 
                                      activity_type == "Museum" | 
                                      activity_type == "Active" | 
                                      activity_type == "Swimming" | 
                                      activity_type == "Zoo" | 
                                      activity_type == "Ice cream" | 
                                      activity_type == "Amusement Park",
                                    select = c(id, partner_postcode, activity_nb, activity_type))
                                      
# Summarize the number of users per day per activity
dt.rPas.activity.mostPop <- dt.rPas.activity.mostPop[, freq_per_pc := .N, by = partner_postcode]

dt.rPas.activity.mostPop <- dt.rPas.activity.mostPop[, c("partner_postcode", "activity_type", "freq_per_pc")]

# Deleting duplicates
dt.rPas.activity.mostPop <- dt.rPas.activity.mostPop[!duplicated(dt.rPas.activity.mostPop), ]

# load and prepare geo tag dataset
ds.zipcodes.geoloc <- read.csv(paste0(dir.providedData, 
                                      "Postalcodes_with_GeoLoc.csv"))
dt.zipcodes.geoloc <- as.data.table(ds.zipcodes.geoloc)
colnames(dt.zipcodes.geoloc)[2] <- "partner_postcode"

#merge the two datasets
dt.rPas.activity.mostPop <- merge(dt.rPas.activity.mostPop, 
                          dt.zipcodes.geoloc, 
                          by = "partner_postcode", 
                          all.x = TRUE)

dt.rPas.activity.mostPop <- dt.rPas.activity.mostPop[, c("freq_per_pc", 
                                                         "activity_type",
                                                         "location.lat", 
                                                         "location.lng")]

# Deleting NA geocodes
dt.rPas.activity.mostPop <- dt.rPas.activity.mostPop[!is.na(dt.rPas.activity.mostPop$location.lat), ]
dt.rPas.activity.mostPop <- dt.rPas.activity.mostPop[!is.na(dt.rPas.activity.mostPop$location.lng), ]

# Order by frequency
dt.rPas.activity.mostPop <- dt.rPas.activity.mostPop[order(freq_per_pc), ]



#### Create map for 2017 ####

# Load saved map of Rotterdam 
load(paste0(dir.results, "map.rotterdam.02.Rda"))

map.activity.pc.mostPop <- ggmap(map.rotterdam.02)
map.activity.pc.mostPop

# Add the activity information from the RotterdamPas dataset
map.activity.pc.mostPop <- map.activity.pc.mostPop + 
  geom_point(aes(x = location.lng, 
                 y = location.lat, 
                 colour = cut(freq_per_pc,
                              c(0, 10, 100, 1000, 10000, 30000, 75000, Inf),
                              labels = c("<= 10","11 - 100", "101 - 1,000", "1,001 - 10,000",
                                         "10,001 - 30,000", "30,001 - 75,000", 
                                         "> 75,000")), 
                 shape = activity_type
                 ),
             data = dt.rPas.activity.mostPop, 
             size = 1.5, 
  ) + 
  scale_colour_brewer(palette = "YlOrRd") + 
  scale_shape_manual(values = c(1:7)) +
  xlab(label = "Longitude") + 
  ylab(label = "Latitude") +
  labs(title = "Most Popular Activities", 
       colour = "Number of Users", 
       shape = "Type of Activity") + 
  theme(plot.title = element_text(hjust = 0.5, color = "#666666"),
        plot.subtitle = element_text(hjust = 0.5, color = "#666666"))

map.activity.pc.mostPop

ggsave(paste0(dir.results, "map.activity.pc.mostPop.pdf"))
