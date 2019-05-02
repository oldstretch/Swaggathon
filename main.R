# install and import the necessary packages

# install.packages("knitr", dependencies = TRUE)
library(knitr)


# set working directory 

# # Elena's working directory
# dir <- "C:/Users/elena/OneDrive/Desktop/Big Data and Business Analytics/Swaggathon/"


# Felix's working directory
dir <- "/Users/felixmeindl/Documents/GitHub/Swaggathon/"


dir.providedData <- paste0(dir, "providedData/")
dir.additionalData <- paste0(dir, "additionalData/")
dir.results <- paste0(dir, "results/")


# set global seed
set.seed(1234)



# save script as pdf
knitr::stitch('main.R')