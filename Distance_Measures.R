### Distance Measure Calculation

install.packages("geosphere")
library(geosphere)

########
# dt.rotterdamPas <- readRDS(paste0("/Users/ulifretzen/Swaggathon/providedData/dt.rotterdamPas.RData"))
# 
# post.code <- read.csv(paste0("/Users/ulifretzen/Swaggathon/providedData/Postalcodes_with_GeoLoc.csv"))
# 
# post.code.red <- post.code[, c("postalcode", "location.lat", "location.lng")]
# post.code.red <- post.code.red[unique(post.code.red$postalcode), ]
# 
# dt.rotterdamPas <- dt.rotterdamPas[!is.na(dt.rotterdamPas$partner_neighborhood), ]
# dt.rotterdamPas <- dt.rotterdamPas[!is.na(dt.rotterdamPas$passH_postcode), ]
# 
# post.code.red <- post.code.red[-5015, ]
# dt.rotterdamPas <- dt.rotterdamPas[-1366789, ]
# 
# post.code.red6 <- post.code.red[nchar(as.character(post.code.red$postalcode)) == 6, ]
# post.code.red6 <- post.code.red6[!is.na(post.code.red6$location.lat), ]
# 
# dt.pas.and.locs <- merge(dt.rotterdamPas, 
#                          post.code.red, 
#                          by.x = "passH_postcode", 
#                          by.y = "postalcode",
#                          all.x = TRUE,
#                          all.y = FALSE)
# colnames(dt.pas.and.locs)[29:30] <- c("lat.passH", "lng.passH")
# 
# dt.pas.and.locs <- merge(dt.pas.and.locs, 
#                          post.code.red, 
#                          by.x = "partner_postcode", 
#                          by.y = "postalcode",
#                          all.x = TRUE,
#                          all.y = FALSE)
# colnames(dt.pas.and.locs)[31:32] <- c("lat.partner", "lng.partner")
# 
# if(is.na(dt.pas.and.locs$lat.partner)) {
#   dt.pas.and.locs$partner_postcode <- post.code.red6[
#     min(which(substr(post.code.red6$postalcode, 1, 4) == 
#             substr(dt.pas.and.locs$partner_postcode, 1, 4))), ]$postalcode
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
# 
# dt.pas.and.locs <- merge(dt.pas.and.locs, 
#                          post.code.red, 
#                          by.x = "passH_postcode", 
#                          by.y = "postalcode",
#                          all.x = TRUE,
#                          all.y = FALSE)
# colnames(dt.pas.and.locs)[29:30] <- c("lat.passH", "lng.passH")
# 
# dt.pas.and.locs <- merge(dt.pas.and.locs, 
#                          post.code.red, 
#                          by.x = "partner_postcode", 
#                          by.y = "postalcode",
#                          all.x = TRUE,
#                          all.y = FALSE)
# colnames(dt.pas.and.locs)[31:32] <- c("lat.partner", "lng.partner")
# colSums(is.na(dt.pas.and.locs))
# 
# dt.pas.and.locs <- dt.pas.and.locs[!is.na(dt.pas.and.locs$lng.passH), ]

#######

library(geosphere)
  
dt.pas.and.locs$distance <- NA
dt.pas.and.locs$loc.comb <- paste(dt.pas.and.locs$passH_postcode, 
                                  dt.pas.and.locs$partner_postcode, sep = "")

v.combination <- c(rep(NA, length(unique(dt.pas.and.locs$loc.comb))))
dt.post.code.combo.unique <- as.data.frame(v.combination)
colnames(dt.post.code.combo.unique) <- "combination"
dt.post.code.combo.unique$combination <- unique(dt.pas.and.locs$loc.comb)

for (i in 1:nrow(dt.post.code.combo.unique)) {
  if (grepl("[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]+",
            dt.post.code.combo.unique$combination[i])){
    dt.post.code.combo.unique$passH_postcode[i] <- 
      substr(dt.post.code.combo.unique$combination[i], 1, 4)
    dt.post.code.combo.unique$partner_postcode[i] <- 
      substr(dt.post.code.combo.unique$combination[i], 5, 8)
  }
  if (grepl("[0-9][0-9][0-9][0-9][A-Z][A-Z][0-9][0-9][0-9][0-9]+",
            dt.post.code.combo.unique$combination[i])){
    dt.post.code.combo.unique$passH_postcode[i] <- 
      substr(dt.post.code.combo.unique$combination[i], 1, 6)
    dt.post.code.combo.unique$partner_postcode[i] <- 
      substr(dt.post.code.combo.unique$combination[i], 7, 10)
  }
  if (grepl("[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][A-Z][A-Z]+",
            dt.post.code.combo.unique$combination[i])){
    dt.post.code.combo.unique$passH_postcode[i] <- 
      substr(dt.post.code.combo.unique$combination[i], 1, 4)
    dt.post.code.combo.unique$partner_postcode[i] <- 
      substr(dt.post.code.combo.unique$combination[i], 5, 10)
  }
  if (grepl("[0-9][0-9][0-9][0-9][A-Z][A-Z][0-9][0-9][0-9][0-9][A-Z][A-Z]+", 
              dt.post.code.combo.unique$combination[i])){
    dt.post.code.combo.unique$passH_postcode[i] <- 
      substr(dt.post.code.combo.unique$combination[i], 1, 6)
    dt.post.code.combo.unique$partner_postcode[i] <- 
      substr(dt.post.code.combo.unique$combination[i], 7, 12)
  }
}


dt.post.code.combo.unique <- merge(dt.post.code.combo.unique,
                                   post.code.red,
                                   by.x = "passH_postcode",
                                   by.y = "postalcode",
                                   all.x = TRUE)
colnames(dt.post.code.combo.unique)[4:5] <- c("lat.passH", "lng.passH")

  
dt.post.code.combo.unique <- merge(dt.post.code.combo.unique, 
                           post.code.red, 
                           by.x = "partner_postcode", 
                           by.y = "postalcode",
                           all.x = TRUE,
                           all.y = FALSE)
colnames(dt.post.code.combo.unique)[6:7] <- c("lat.partner", "lng.partner")


for (i in 1:nrow(dt.post.code.combo.unique)) { 
dt.post.code.combo.unique$distance[i] <- distm(c(dt.post.code.combo.unique$lng.passH[i], 
                                              dt.post.code.combo.unique$lat.passH[i]), 
                                            c(dt.post.code.combo.unique$lng.partner[i],
                                              dt.post.code.combo.unique$lat.partner[i]), 
                                            fun = distHaversine)
}

dt.pas.and.locs <- merge(dt.pas.and.locs, 
                         dt.post.code.combo.unique,
                         by.x = "loc.comb",
                         by.y = "combination",
                         all.x = TRUE)


# save script as pdf
knitr::stitch('Distance_Measures.R')


