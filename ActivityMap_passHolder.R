# load rotterdam pas data
ds.rPas.passHolder <- readRDS(paste0(dir.providedData, 
                                           "dt.rotterdamPas.RData"))
dt.rPas.passHolder <- as.data.table(ds.rPas.passHolder)

dt.rPas.passHolder <- dt.rPas.passHolder[, "passHolder_within_rotterdam" := ifelse(substr(passH_p4, 1, 2) == 30, 1, 0)]
dt.rPas.passHolder$passHolder_within_rotterdam <- factor(dt.rPas.passHolder$passHolder_within_rotterdam)

#Reduce dataset to individual passholders (not transactions)
dt.rPas.passHolder <- dt.rPas.passHolder[!duplicated(dt.rPas.passHolder$passH_nb), ]



#### Create piechart that shows whether passHolder are from within Rotterdam ####

# Summarize the number of passholders that use
dt.rPas.passHolder.piechart <- dt.rPas.passHolder[, freq_Rtown := .N, by = passHolder_within_rotterdam]

dt.rPas.passHolder.piechart <- dt.rPas.passHolder.piechart[, c("passHolder_within_rotterdam", "freq_Rtown")]

# Deleting duplicates
dt.rPas.passHolder.piechart <- dt.rPas.passHolder.piechart[!duplicated(dt.rPas.passHolder.piechart), ]

# Deleting NA 
dt.rPas.passHolder.piechart <- 
  dt.rPas.passHolder.piechart[!is.na(dt.rPas.passHolder.piechart$passHolder_within_rotterdam), ]

# labeling
dt.rPas.passHolder.piechart$passHolder_within_rotterdam <- 
  factor(dt.rPas.passHolder.piechart$passHolder_within_rotterdam, 
         levels = c(0, 1), 
         labels = c("outside of Rotterdam", 
                    "within Rotterdam"))


# Create pie chart
PieChart.passHolder <- ggplot(dt.rPas.passHolder.piechart, 
                              aes(x = "", 
                                  y = freq_Rtown, 
                                  fill = passHolder_within_rotterdam)) + 
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start = 0) +
  scale_fill_manual(values = c("#FEB24C", "#B10026")) +
  labs(title = "Pass Holders from within Rotterdam",
       subtitle = "vs. from outside Rotterdam",
       fill = "Postcode of Pass Holders",
       y = "", 
       x = "") +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "#666666"),
        plot.subtitle = element_text(hjust = 0.5, color = "#666666")) +
  geom_text(aes(label = paste0(round(freq_Rtown/226077, 1), "%")), position = position_stack(vjust = 0.5))

# Plot pie chart
PieChart.passHolder

# Save pie chart
ggsave(paste0(dir.results, "PieChart.passHolder.pdf"))




#### Prepare Data for Mapping of Pass Holders within Rotterdam (Heatmap) ####


dt.rPas.passHolder.RTown <- dt.rPas.passHolder[passHolder_within_rotterdam == 1, ]

# Summarize the number of pass holders per postcode
dt.rPas.passHolder.RTown <- dt.rPas.passHolder.RTown[, 
                                                     freq_per_pc := .N, 
                                                     by = passH_postcode]

dt.rPas.passHolder.RTown <- dt.rPas.passHolder.RTown[, c("passH_postcode", 
                                                         "freq_per_pc")]

# Deleting duplicates
dt.rPas.passHolder.RTown <- dt.rPas.passHolder.RTown[!duplicated(dt.rPas.passHolder.RTown), ]

# load and prepare geo tag dataset
ds.zipcodes.geoloc <- read.csv(paste0(dir.providedData, 
                                      "Postalcodes_with_GeoLoc.csv"))
dt.zipcodes.geoloc <- as.data.table(ds.zipcodes.geoloc)
colnames(dt.zipcodes.geoloc)[2] <- "passH_postcode"

#merge the two datasets
dt.rPas.passHolder.RTown <- merge(dt.rPas.passHolder.RTown, 
                                  dt.zipcodes.geoloc, 
                                  by = "passH_postcode", 
                                  all.x = TRUE)


dt.rPas.passHolder.RTown <- dt.rPas.passHolder.RTown[, c("freq_per_pc", 
                                                         "location.lat", 
                                                         "location.lng")]

# Deleting NA geocodes
dt.rPas.passHolder.RTown <- dt.rPas.passHolder.RTown[!is.na(dt.rPas.passHolder.RTown$location.lat), ]
dt.rPas.passHolder.RTown <- dt.rPas.passHolder.RTown[!is.na(dt.rPas.passHolder.RTown$location.lng), ]

# Order by frequency
dt.rPas.passHolder.RTown <- dt.rPas.passHolder.RTown[order(freq_per_pc), ]

View(dt.rPas.passHolder.RTown)



#### Create map with Pass Holders within Rotterdam ####

# Load saved map of Rotterdam 
load(paste0(dir.results, "map.rotterdam.02.Rda"))

map.passHolder.RTown <- ggmap(map.rotterdam.02)
map.passHolder.RTown

# Add the activity information from the RotterdamPas dataset
map.passHolder.RTown <- map.passHolder.RTown + 
  geom_point(aes(x = location.lng, 
                 y = location.lat, 
                 colour = cut(freq_per_pc,
                              c(0, 5, 10, 25, 50, 100, 200, Inf),
                              labels = c("<= 5","6 - 10", "11 - 25", "26 - 50",
                                         "51 - 100", "101 - 200", 
                                         "> 200"))
  ),
  data = dt.rPas.passHolder.RTown, 
  size = 1.5, 
  ) + 
  scale_colour_brewer(palette = "YlOrRd") + 
  xlab(label = "Longitude") + 
  ylab(label = "Latitude") +
  labs(title = "Pass Holders Within Rotterdam", 
       colour = "# per Postcode") +  
  theme(plot.title = element_text(hjust = 0.5, color = "#666666"),
        plot.subtitle = element_text(hjust = 0.5, color = "#666666"))

map.passHolder.RTown

ggsave(paste0(dir.results, "map.passHolder.RTown.pdf"))



#### Prepare Data for Mapping of most common Postcode - Age category combinations ####

dt.rPas.passHolder.RTown.age <- dt.rPas.passHolder[passHolder_within_rotterdam == 1, ]

# Summarize the number of pass holders per postcode
dt.rPas.passHolder.RTown.age <- dt.rPas.passHolder.RTown.age[, 
                                                             freq_per_pc := .N, 
                                                             by = c("passH_postcode", 
                                                                    "age_category")]

dt.rPas.passHolder.RTown.age  <- dt.rPas.passHolder.RTown.age [, 
                                                               c("passH_postcode", 
                                                                 "age_category", 
                                                                 "freq_per_pc")]

# Deleting duplicates
dt.rPas.passHolder.RTown.age <- dt.rPas.passHolder.RTown.age[!duplicated(dt.rPas.passHolder.RTown.age), ]

# Order by frequency
dt.rPas.passHolder.RTown.age <- dt.rPas.passHolder.RTown.age[order(-freq_per_pc), ]

dt.rPas.passHolder.RTown.age <- dt.rPas.passHolder.RTown.age[1:200, ]


# load and prepare geo tag dataset
ds.zipcodes.geoloc <- read.csv(paste0(dir.providedData, 
                                      "Postalcodes_with_GeoLoc.csv"))
dt.zipcodes.geoloc <- as.data.table(ds.zipcodes.geoloc)
colnames(dt.zipcodes.geoloc)[2] <- "passH_postcode"

#merge the two datasets
dt.rPas.passHolder.RTown.age <- merge(dt.rPas.passHolder.RTown.age, 
                                      dt.zipcodes.geoloc, 
                                      by = "passH_postcode", 
                                      all.x = TRUE)


dt.rPas.passHolder.RTown.age <- dt.rPas.passHolder.RTown.age[, 
                                                             c("freq_per_pc", 
                                                               "age_category",
                                                               "location.lat", 
                                                               "location.lng")]

# Deleting NA geocodes
dt.rPas.passHolder.RTown.age <- 
  dt.rPas.passHolder.RTown.age[!is.na(dt.rPas.passHolder.RTown.age$location.lat), ]

dt.rPas.passHolder.RTown.age <- 
  dt.rPas.passHolder.RTown.age[!is.na(dt.rPas.passHolder.RTown.age$location.lng), ]

# Order by frequency
dt.rPas.passHolder.RTown.age <- dt.rPas.passHolder.RTown.age[order(freq_per_pc), ]

View(dt.rPas.passHolder.RTown.age)


#### Create map with Pass Holders within Rotterdam ####

# Load saved map of Rotterdam 
load(paste0(dir.results, "map.rotterdam.02.Rda"))

map.passHolder.RTown.age <- ggmap(map.rotterdam.02)
map.passHolder.RTown.age

# Add the activity information from the RotterdamPas dataset
map.passHolder.RTown.age <- map.passHolder.RTown.age + 
  geom_point(aes(x = location.lng, 
                 y = location.lat, 
                 colour = cut(freq_per_pc,
                              c(0, 40, 50, 70, 90, 200, Inf),
                              labels = c("<= 40","41 - 50", "51 - 70", 
                                         "71 - 90", "91 - 120", "> 121")), 
                 shape = age_category
  ),
  data = dt.rPas.passHolder.RTown.age, 
  size = 1.5, 
  ) + 
  scale_colour_brewer(palette = "YlOrRd") + 
  scale_shape_manual(values = c(1:6)) +
  xlab(label = "Longitude") + 
  ylab(label = "Latitude") +
  labs(title = "Most common Postcode / Age-Category Combinations", 
       subtitle = "of Pass Holders Within Rotterdam", 
       colour = "# per Postcode", 
       shape = "Age Category") + 
  theme(plot.title = element_text(hjust = 0.5, color = "#666666"),
        plot.subtitle = element_text(hjust = 0.5, color = "#666666"))

map.passHolder.RTown.age

ggsave(paste0(dir.results, "map.passHolder.RTown.age.pdf"))
