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


#### Prepare Data for Mapping of Pass Holders within Rotterdam ####


dt.rPas.passHolder.RTown <- dt.rPas.passHolder[passHolder_within_rotterdam == 1, ]

# Summarize the number of pass holders per postcode
dt.rPas.passHolder.RTown <- dt.rPas.passHolder.RTown[, 
                                                     freq_per_pc := .N, 
                                                     by = c("passH_postcode", 
                                                            "age_category")]

dt.rPas.passHolder.RTown <- dt.rPas.passHolder.RTown[, c("passH_postcode", 
                                                         "age_category", 
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
                                                         "age_category",
                                                         "location.lat", 
                                                         "location.lng")]

# Deleting NA geocodes
dt.rPas.passHolder.RTown <- dt.rPas.passHolder.RTown[!is.na(dt.rPas.passHolder.RTown$location.lat), ]
dt.rPas.passHolder.RTown <- dt.rPas.passHolder.RTown[!is.na(dt.rPas.passHolder.RTown$location.lng), ]

# Order by frequency
dt.rPas.passHolder.RTown <- dt.rPas.passHolder.RTown[order(freq_per_pc), ]

View(dt.rPas.passHolder.RTown)

