#-------------------------------------------------------------------------------------------
#                                  City Center Visits Database
#-------------------------------------------------------------------------------------------

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


