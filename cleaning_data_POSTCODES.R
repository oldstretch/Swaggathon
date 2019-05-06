##### City Center Visits Database #####

library(readr)
library(lubridate)
library(data.table)

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

ds.events <- read.csv(paste0(dir.providedData, "ds.urban.events.csv"))


##### Clean / Understand Rotterdampas dataset #####

load(paste0(dir.providedData, "rotterdampas.RData"))
# load(paste0("/Users/ulifretzen/Swaggathon/providedData/rotterdampas.RData"))
ds.rotterdamPas <- Rotterdampas_2017_2018

colnames(ds.rotterdamPas) <- c("id", "passH_nb", "age_category", "passH_postcode", 
                               "passH_p4", "passH_neighborhood", "passH_district", "partner_nb",
                               "partner_postcode", "partner_p4", "partner_neighborhood", "partner_district",
                               "activity_nb", "discount", "activity_validity", "inside", 
                               "nice_weather", "bad_weather", "fun_for_kids", "fun_without_kids", 
                               "highlight", "use_date", "compensation_incl_tax", "social_group", 
                               "activity_category", "activity_type", "year", "time")

# limit rotterdamPas dataset to activities with partners that are located within rotterdam (based on 30XX postcode)
library(data.table)
dt.rotterdamPas <- as.data.table(ds.rotterdamPas)
dt.rotterdamPas <- dt.rotterdamPas[, "activity_within_rotterdam" := ifelse(substr(partner_p4, 1, 2) == 30, 1, 0)]
dt.rotterdamPas$activity_within_rotterdam <- factor(dt.rotterdamPas$activity_within_rotterdam)
dt.rotterdamPas <- dt.rotterdamPas[activity_within_rotterdam == 1, ]

dt.rotterdamPas$partner_postcode <- dt.rotterdamPas[, gsub(" ", "", dt.rotterdamPas$partner_postcode)]

# save dataset
saveRDS(dt.rotterdamPas, file = paste0(dir.providedData, "dt.rotterdamPas.RData"))


# XXXXXXXXXXXXXXXXX IMPORT

load(paste0(dir.providedData, "rotterdampas.RData"))
ds.rotterdamPas <- Rotterdampas_2017_2018
colnames(ds.rotterdamPas) <- c("id", "passH_nb", "age_category", "passH_postcode", 
                               "passH_p4", "passH_neighborhood", "passH_district", "partner_nb",
                               "partner_postcode", "partner_p4", "partner_neighborhood", "partner_district",
                               "activity_nb", "discount", "activity_validity", "inside", 
                               "nice_weather", "bad_weather", "fun_for_kids", "fun_without_kids", 
                               "highlight", "use_date", "compensation_incl_tax", "social_group", 
                               "activity_category", "activity_type", "year", "time")

library(data.table)
dt.rotterdamPas <- as.data.table(ds.rotterdamPas)
dt.rotterdamPas <- dt.rotterdamPas[, "activity_within_rotterdam" := ifelse(substr(partner_p4, 1, 2) == 30, 1, 0)]
dt.rotterdamPas$activity_within_rotterdam <- factor(dt.rotterdamPas$activity_within_rotterdam)
dt.rotterdamPas <- dt.rotterdamPas[activity_within_rotterdam == 1, ]

dt.rotterdamPas$partner_postcode <- dt.rotterdamPas[, gsub(" ", "", dt.rotterdamPas$partner_postcode)]



# dt.rotterdamPas <- readRDS(paste0("/Users/ulifretzen/Swaggathon/providedData/dt.rotterdamPas.RData"))

# post.code <- read.csv(paste0("/Users/ulifretzen/Swaggathon/providedData/Postalcodes_with_GeoLoc.csv"))

post.code <- read.csv(paste0(dir.providedData,
                             "Postalcodes_with_GeoLoc.csv"))

post.code.red <- post.code[, c("postalcode", "location.lat", "location.lng")]
post.code.red <- post.code.red[unique(post.code.red$postalcode), ]

dt.rotterdamPas <- dt.rotterdamPas[!is.na(dt.rotterdamPas$partner_neighborhood), ]
dt.rotterdamPas <- dt.rotterdamPas[!is.na(dt.rotterdamPas$passH_postcode), ]

post.code.red <- post.code.red[-5015, ]
dt.rotterdamPas <- dt.rotterdamPas[-1366789, ]

post.code.red6 <- post.code.red[nchar(as.character(post.code.red$postalcode)) == 6, ]
post.code.red6 <- post.code.red6[!is.na(post.code.red6$location.lat), ]

dt.pas.and.locs <- merge(dt.rotterdamPas,
                         post.code.red,
                         by.x = "passH_postcode",
                         by.y = "postalcode",
                         all.x = TRUE,
                         all.y = FALSE)
colnames(dt.pas.and.locs)[30:31] <- c("lat.passH", "lng.passH")

dt.pas.and.locs <- merge(dt.pas.and.locs,
                         post.code.red,
                         by.x = "partner_postcode",
                         by.y = "postalcode",
                         all.x = TRUE,
                         all.y = FALSE)

colnames(dt.pas.and.locs)[32:33] <- c("lat.partner", "lng.partner")

# if(is.na(dt.pas.and.locs$lat.partner)) {
#   dt.pas.and.locs$partner_postcode <- post.code.red6[
#     min(which(substr(post.code.red6$postalcode, 1, 4) ==
#                 substr(dt.pas.and.locs$partner_postcode, 1, 4))), ]$postalcode
# }
# 
# if(is.na(dt.pas.and.locs$lat.passH)) {
#   dt.pas.and.locs$passH_postcode <- post.code.red6[
#     min(which(substr(post.code.red6$postalcode, 1, 4) ==
#                 substr(dt.pas.and.locs$passH_postcode, 1, 4))), ]$postalcode
# }
# 
# dt.pas.and.locs$lat.passH <- NULL
# dt.pas.and.locs$lng.passH <- NULL
# dt.pas.and.locs$lat.partner <- NULL
# dt.pas.and.locs$lng.partner <- NULL
# head(dt.pas.and.locs)
# dt.pas.and.locs <- merge(dt.pas.and.locs,
#                          post.code.red,
#                          by.x = "passH_postcode",
#                          by.y = "postalcode",
#                          all.x = TRUE,
#                          all.y = FALSE)
# colnames(dt.pas.and.locs)[30:31] <- c("lat.passH", "lng.passH")
# 
# dt.pas.and.locs <- merge(dt.pas.and.locs,
#                          post.code.red,
#                          by.x = "partner_postcode",
#                          by.y = "postalcode",
#                          all.x = TRUE,
#                          all.y = FALSE)
# colnames(dt.pas.and.locs)[32:33] <- c("lat.partner", "lng.partner")


# save dataset
saveRDS(dt.rotterdamPas, file = paste0(dir.providedData, "dt.rotterdamPas.RData"))





# XXXXXXXXXXXXXXXXX IMPORT






# Compensation is what the government pays which the people don't, in order to provide the discount
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


##### Import and translate postalcodes_with geoloc #####

ds.postalCodes <- read.csv(paste0(dir.providedData, "Postalcodes_with_GeoLoc.csv"))

##### Import and prepare weather data #####

library(stringr)

ds.weather <- read.delim(paste0(dir.additionalData, "ds.weather.txt"))
df.weather <- as.data.frame(ds.weather)

names(df.weather) <- c("wt")

df.weather <- str_split_fixed(df.weather$wt, ",", 12)
df.weather <- df.weather[19:1115, 2:12]
df.weather <- df.weather[, c(-3,-5,-6,-8,-10)]
df.weather <- as.data.frame(df.weather)
names(df.weather) <- c("Date", 
                       "Daily Avg. Wind Speed", 
                       "Daily Avg. Temperature", 
                       "Sunshine Duration", 
                       "Prec. Duration",
                       "Highest h. amount prec.")
df.weather <- df.weather[3:1097, ]
df.weather$Date <- as.character(df.weather$Date)
df.weather$Date <- sub("([[:digit:]]{4,4})$", "/\\1", df.weather$Date)
df.weather$Date <- sub("(.{7})(/*)", "\\1/\\2", df.weather$Date)
df.weather$Date <- as.Date(df.weather$Date)
saveRDS(df.weather, file = paste0(dir.providedData, "df.weather.RData"))

# save script as pdf
knitr::stitch('cleaning_data.R')
