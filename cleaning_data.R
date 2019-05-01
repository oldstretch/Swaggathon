
##### Clean / Understand Stedelijke_Evenementen dataset #####

# read in dataset
ds.urban.events <- read.csv(paste0(dir.providedData, "Stedelijke_Evenementen_2010_2017.csv"))

# translate columns to english
colnames(ds.urban.events) <- c("id", 
                               "event_name", 
                               "organizer", 
                               "entree_fee", 
                               "initial_year", 
                               "start_date", 
                               "end_date", 
                               "nr_days", 
                               "location", 
                               "inside_or_outside",
                               "international_national_regional", 
                               "nr_visitors",
                               "year")


View(ds.urban.events)

write.csv(ds.urban.events, file = paste0(dir.providedData, "ds.urban.events.csv"), row.names = FALSE)


##### Clean / Understand Rotterdampas dataset #####

# load(paste0(dir.providedData, "rotterdampas.RData"))
# 
# ds.rotterdamPas <- rotterdampas
# 
# View(ds.rotterdamPas)
