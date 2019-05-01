# install and import the necessary packages

# install.packages("knitr", dependencies = TRUE)
library(knitr)


# set working directory (individual per teammember!!!)

# Elena's working directory
# setwd("XXX")

# Edwin's working directory
# setwd("XXX")

# Lisa's working directory
# setwd("XXX")

# Felix's working directory
dir <- "/Users/felixmeindl/Documents/GitHub/Swaggathon/"

# Uli's working directory
# setwd("XXX")


dir.providedData <- paste0(dir, "providedData/")
dir.additionalData <- paste0(dir, "additionalData/")
dir.results <- paste0(dir, "results/")


# set global seed
set.seed(1234)


# run all hackathon scripts
# source("data_cleaning.R")


# save script as pdf
# knitr::stitch('main.R')