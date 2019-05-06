
rmarkdown::render("main.R", 
                  "pdf_document", 
                  paste0(dir.results, "main.pdf"), 
                  output_options = list(pandoc_args = c("--metadata=title:\"Appendix: main.R\"", 
                                                        "--metadata=author:\"Group13\"", 
                                                        "--metadata=date:\"07.05.2019\"")), 
                  clean = TRUE)


# set working directory 

# # Elena's working directory
# dir <- "C:/Users/elena/OneDrive/Desktop/Big Data and Business Analytics/Swaggathon/"

# Uli's working directory
dir <- "/Users/ulifretzen/Swaggathon/"

# Felix's working directory
dir <- "/Users/felixmeindl/Documents/GitHub/Swaggathon/"


dir.providedData <- paste0(dir, "providedData/")
dir.additionalData <- paste0(dir, "additionalData/")
dir.results <- paste0(dir, "results/")


# set global seed
set.seed(1234)

