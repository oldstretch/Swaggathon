### Distance Measure Calculation

install.packages("geosphere")
library(geosphere)

ds.rotterdamPas <- readRDS(paste0("/Users/ulifretzen/Swaggathon/providedData/ds.rotterdamPas.RData"))

post.code <- read.csv(paste0("/Users/ulifretzen/Swaggathon/providedData/Postalcodes_with_GeoLoc.csv"))

post.code.red <- post.code[, c("postalcode", "location.lat", "location.lng")]

post.code.red.passH <- post.code.red
colnames(post.code.red.passH)[2:3] <- c("lat.passH", "lng.passH")

ds.pas.and.locs <- merge(ds.rotterdamPas, post.code.red.passH, by.x = "passH_postcode", by.y = "postalcode")

post.code.red. <- post.code.red

colnames(ds.pas.and.locs <- "lat.passH"
colnames(ds)

distm(c(ds.pas.and.locs$, ds.pas.and.locs), c(lon2, lat2), fun = distHaversine)

head(ds.rotterdamPas)
nrow(ds.rotterdamPas)

View(ds.rotterdamPas)
View(post.code)
View(ds.pas.and.locs)

