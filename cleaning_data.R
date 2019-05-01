# Clean sport data: Sportparticipatie_Rotterdam_2015_2017.csv
sportPart.ds <- read_csv("providedData/Sportparticipatie_Rotterdam_2015_2017.csv")

View(sportPart.ds)

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

View(sportPart.ds)

# Save cleaned data
write.csv(sportPart.ds,'providedData/cleanSports.csv')
