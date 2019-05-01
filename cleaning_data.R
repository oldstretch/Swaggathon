##### City Center Visits Database #####

library(readr)
library(lubridate)

Visit_Frequencies_Q1_2017 <- read_delim("providedData/Visit Frequencies Q1 2017.csv",
                                        ";", escape_double = FALSE, 
                                        col_types = cols(Average_Time = col_time(format = "%H:%M:%S")),
                                        trim_ws = TRUE)
Visit_Frequencies_Q2_2017 <- read_delim("providedData/Visit Frequencies Q2 2017.csv",
                                        ";", escape_double = FALSE, 
                                        col_types = cols(Average_Time = col_time(format = "%H:%M:%S")),
                                        trim_ws = TRUE)
Visit_Frequencies_Q3_2017 <- read_delim("providedData/Visit Frequencies Q3 2017.csv",
                                        ";", escape_double = FALSE, 
                                        col_types = cols(Average_Time = col_time(format = "%H:%M:%S")),
                                        trim_ws = TRUE)
Visit_Frequencies_Q4_2016 <- read_delim("providedData/Visit Frequencies Q4 2016.csv",
                                        ";", escape_double = FALSE, 
                                        col_types = cols(Average_Time = col_time(format = "%H:%M:%S")),
                                        trim_ws = TRUE)

Visit_Times_Q1_2017 <- read_delim("providedData/Visit Times Q1 2017.csv",
                                  ";", escape_double = FALSE, 
                                  trim_ws = TRUE)
Visit_Times_Q2_2017 <- read_delim("providedData/Visit Times Q2 2017.csv",
                                  ";", escape_double = FALSE, 
                                  trim_ws = TRUE)
Visit_Times_Q3_2017 <- read_delim("providedData/Visit Times Q3 2017.csv",
                                  ";", escape_double = FALSE, 
                                  trim_ws = TRUE)
Visit_Times_Q4_2016 <- read_delim("providedData/Visit Times Q4 2016.csv",
                                  ";", escape_double = FALSE, 
                                  trim_ws = TRUE)

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

write.csv(ds.urban.events, file = paste0(dir.providedData, "ds.urban.events.csv"), row.names = FALSE)


##### Clean / Understand Rotterdampas dataset #####

# load(paste0(dir.providedData, "rotterdampas.RData"))
# 
# ds.rotterdamPas <- rotterdampas
# 
# View(ds.rotterdamPas)



##### Clean sport data: Sportparticipatie_Rotterdam_2015_2017.csv #####
sportPart.ds <- read_csv("providedData/Sportparticipatie_Rotterdam_2015_2017.csv")

sportPart.ds <- sportPart.ds[, 2:ncol(sportPart.ds)]

colnames(sportPart.ds) <- c("Neighbourhood",
                            "Postcode",
                            "Year",
                            "Total %",
                            "4-11 years %",
                            "12-17 years %",
                            "18-64 years %",
                            "65-80 years %",
                            "81+ years %",
                            "4-11 years % men",
                            "4-11 years % women",
                            "12-17 years % men",
                            "12-17 years % women",
                            "18-64 years % men",
                            "18-64 years % women",
                            "65-80 years % men",
                            "65-80 years % women",
                            "81+ years % men",
                            "81+ years % women")

# Save cleaned data
write.csv(sportPart.ds,'providedData/cleanSports.csv')

