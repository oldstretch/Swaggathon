library(data.table)

dsRotterdamPas <- readRDS("./providedData/ds.rotterdamPas.RData")

dfPostalCodes <- read.csv2("./providedData/Postalcodes_with_GeoLoc.csv", sep = ",")
dfPostalCodes <- dfPostalCodes[, c(2, 4:5)]

dsRotterdamPasNei <- dsRotterdamPas[, c("use_date", "passH_district", "partner_district")]
dsRotterdamPasNei$use_date <- as.numeric(format(dsRotterdamPasNei$use_date, "%m"))
setDT(dsRotterdamPasNei)

dsPartnerNei <- dsRotterdamPasNei[, c(1, 3)]

setnames(dsPartnerNei, "partner_district", "id")

dsPartnerNei[id == 	"'s-Gravenhage"]$id <- "'s Gravenland"
dsPartnerNei[id == 	"Charlois Zuidrand/Wielewaal/Zuiderpark"]$id <- "Charlois"
dsPartnerNei[id == 	"Crooswijk"]$id <- "Oud Crooswijk"
dsPartnerNei[id == 	"De Esch/Struisenburg"]$id <- "De Esch"
dsPartnerNei[id == 	"Dorp/Strand en Duin/Rijnpoort"]$id <- "Dorp"
dsPartnerNei[id == "Het Lage Land/Oosterflank"]$id <- "Oosterflank"
dsPartnerNei[id == "Hoogvliet-Zuid"]$id <- "Hoogvliet Zuid"

dsPartnerNei[, count := .N, by = .(id, use_date)]

KopKatAfr <- dsPartnerNei[id == "Kop van Zuid/Katendrecht/Afrikaanderwijk"]
kop <- KopKatAfr
kop$id <- "Kop van Zuid"
kop$count <- kop$count/3
kat <- KopKatAfr
kat$id <- "Katendrecht"
kat$count <- kat$count/3
afr <- KopKatAfr
afr$id <- "Afrikaanderwijk"
afr$count <- afr$count/3
dsPartnerNei <- rbind(dsPartnerNei, kop, kat, afr)
dsPartnerNei <- dsPartnerNei[-which(dsPartnerNei$id == "Kop van Zuid/Katendrecht/Afrikaanderwijk"), ]

Kralingen <- dsPartnerNei[id == "Kralingen Oost/Bos"]
oost <- Kralingen
oost$id <- "Kralingen Oost"
oost$count <- oost$count/2
bos <- Kralingen
bos$id <- "Kralingse Bos"
bos$count <- bos$count/2
dsPartnerNei <- rbind(dsPartnerNei, oost, bos)
dsPartnerNei <- dsPartnerNei[-which(dsPartnerNei$id == "Kralingen Oost/Bos"), ]

ommzev <- dsPartnerNei[id == "Ommoord/Zevenkamp"]
omm <- ommzev
omm$id <- "Ommoord"
omm$count <- omm$count/2
zev <- ommzev
zev$id <- "Zevenkamp"
zev$count <- zev$count/2
dsPartnerNei <- rbind(dsPartnerNei, omm, zev)
dsPartnerNei <- dsPartnerNei[-which(dsPartnerNei$id == "Ommoord/Zevenkamp"), ]

oudbev <- dsPartnerNei[id == "Oud IJsselmonde/Beverwaard"]
oud <- oudbev
oud$id <- "Oud IJsselmonde"
oud$count <- oud$count/2
bev <- oudbev
bev$id <- "Beverwaard"
bev$count <- bev$count/2
dsPartnerNei <- rbind(dsPartnerNei, oud, bev)
dsPartnerNei <- dsPartnerNei[-which(dsPartnerNei$id == "Oud IJsselmonde/Beverwaard"), ]
dsPartnerNei <- dsPartnerNei[-which(dsPartnerNei$id == "Hoogvliet Zuid"), ]

dsPartnerNei <- unique(dsPartnerNei)
dsPartnerNei <- dsPartnerNei[complete.cases(dsPartnerNei), ]

library(data.table)
library(ggmap)
library(rgdal)
library(raster)
library(reshape2)
library(gganimate)
library(maptools)

##########################################################################################################
#                                       MAP PARTNER CONCENTRATION MAP

dfShape <- readShapeSpatial("./providedData/ShapeBuurtWijk/buurt2018.shp", proj4string = CRS("+init=epsg:28992")) 
dfShape <- spTransform(dfShape, CRS("+init=epsg:28992"))
dfShape <- spTransform(dfShape, CRS("+proj=longlat +datum=WGS84"))

dfShape.Rdam  <- dfShape[!is.na(dfShape@data$GM_NAAM) &
                           dfShape@data$GM_NAAM == "Rotterdam",]

clipRdam <- as(extent(4.3, 4.6, 50, 52), "SpatialPolygons")
proj4string(clipRdam) <- CRS(proj4string(dfShape.Rdam))
dfShape.RdamClip <- intersect(dfShape.Rdam, clipRdam)

dfData.RdamClip <- fortify(dfShape.RdamClip, region = "BU_NAAM")

dsPartnerNeiSpat <- merge(dsPartnerNei, data.frame(dfData.RdamClip), 
                          by = "id", 
                          allow.cartesian = TRUE)

ggplot(data = dsPartnerNeiSpat[use_date == "Feb"], aes(x = long, y = lat, group = group)) +
  geom_polygon() +
  scale_fill_continuous(low="white", high="red")

# Boundaries
minLat <- min(dsPartnerNeiSpat$lat)
maxLat <- max(dsPartnerNeiSpat$lat)
minLon <- min(dsPartnerNeiSpat$long)
maxLon <- max(dsPartnerNeiSpat$long)

rangeLat <- maxLat - minLat
rangeLon <- maxLon - minLon

# Set the bounding box 
mrg    <- 0.10   
bbox <- 
  c(minLon - mrg*rangeLon, minLat - mrg*rangeLat, 
    maxLon + mrg*rangeLon, maxLat + mrg*rangeLat)

# Get the map
myMap <- 
  get_stamenmap(bbox, zoom = 11, maptype = "terrain")

save(myMap, file="mapRotterdam01.Rda")
load("mapRotterdam01.Rda")

p01 <- ggplot(dsPartnerNeiSpat, aes(x = long, y = lat, group = group,
                   fill = cut(count, c(0, 10, 100, 500, 2000, 10000, 30000, Inf), 
                              labels = c("0-10", "10-100", "100-500", "500-2,000", "2,000-10,000", "10,000-30,000", "30,000+")))) +
  geom_polygon() +
  scale_fill_brewer(palette = "YlOrRd") +
  labs(title = "Use of the RotterdamPas per Neighborhood over the year",
         fill = "Times used")
  
panim1 <- p01+
  transition_time(use_date)  +
  ggtitle("RotterdamPas used per Neighborhoods, for month: {frame_time}") 

gganimate::animate(panim1, renderer = av_renderer())
anim_save("animatedRotterdamPasUse.mp4")

##########################################################################################################
#                                       MAP USER CONCENTRATION MAP

dsUserNei <- dsRotterdamPasNei[, 1:2]
setnames(dsUserNei, "passH_district", "id")
dsUserNei[id == 	"'s-Gravenhage"]$id <- "'s Gravenland"
dsUserNei[id == 	"Charlois Zuidrand/Wielewaal/Zuiderpark"]$id <- "Charlois"
dsUserNei[id == 	"Crooswijk"]$id <- "Oud Crooswijk"
dsUserNei[id == 	"De Esch/Struisenburg"]$id <- "De Esch"
dsUserNei[id == 	"Dorp/Strand en Duin/Rijnpoort"]$id <- "Dorp"
dsUserNei[id == "Het Lage Land/Oosterflank"]$id <- "Oosterflank"
dsUserNei[id == "Hoogvliet-Zuid"]$id <- "Hoogvliet Zuid"

dsUserNei[, count := .N, by = .(id, use_date)]
dsUserNei[, overallCount := .N, by = "id"]

KopKatAfr <- dsUserNei[id == "Kop van Zuid/Katendrecht/Afrikaanderwijk"]
kop <- KopKatAfr
kop$id <- "Kop van Zuid"
kop$count <- kop$count/3
kat <- KopKatAfr
kat$id <- "Katendrecht"
kat$count <- kat$count/3
afr <- KopKatAfr
afr$id <- "Afrikaanderwijk"
afr$count <- afr$count/3
dsUserNei <- rbind(dsUserNei, kop, kat, afr)
dsUserNei <- dsUserNei[-which(dsUserNei$id == "Kop van Zuid/Katendrecht/Afrikaanderwijk"), ]

Kralingen <- dsUserNei[id == "Kralingen Oost/Bos"]
oost <- Kralingen
oost$id <- "Kralingen Oost"
oost$count <- oost$count/2
bos <- Kralingen
bos$id <- "Kralingse Bos"
bos$count <- bos$count/2
dsUserNei <- rbind(dsUserNei, oost, bos)
dsUserNei <- dsUserNei[-which(dsUserNei$id == "Kralingen Oost/Bos"), ]

ommzev <- dsUserNei[id == "Ommoord/Zevenkamp"]
omm <- ommzev
omm$id <- "Ommoord"
omm$count <- omm$count/2
zev <- ommzev
zev$id <- "Zevenkamp"
zev$count <- zev$count/2
dsUserNei <- rbind(dsUserNei, omm, zev)
dsUserNei <- dsUserNei[-which(dsUserNei$id == "Ommoord/Zevenkamp"), ]

oudbev <- dsUserNei[id == "Oud IJsselmonde/Beverwaard"]
oud <- oudbev
oud$id <- "Oud IJsselmonde"
oud$count <- oud$count/2
bev <- oudbev
bev$id <- "Beverwaard"
bev$count <- bev$count/2
dsUserNei <- rbind(dsUserNei, oud, bev)
dsUserNei <- dsUserNei[-which(dsUserNei$id == "Oud IJsselmonde/Beverwaard"), ]
dsUserNei <- dsUserNei[-which(dsUserNei$id == "Hoogvliet Zuid"), ]

dsUserNei <- unique(dsUserNei)
dsUserNei <- dsUserNei[complete.cases(dsUserNei), ]


dsUserNeiSpat <- merge(dsUserNei, data.frame(dfData.RdamClip), 
                          by = "id", 
                          allow.cartesian = TRUE, 
                       all = TRUE)

RotterdampPasOrigin <- ggplot(data = dsUserNeiSpat, aes(x = long, y = lat, group = group, 
                                                    fill = cut(overallCount, 
                                                               c(0, 10, 100, 500, 2000, 10000, 30000, Inf), 
                                                               labels = c("0-10", "10-100", "100-500", "500-2,000", "2,000-10,000", "10,000-30,000", "30,000+")))) +
  geom_polygon() +
  scale_fill_brewer(palette = "YlOrRd") +
  labs(title = "Origin of the RotterdamPas holder per Neighborhood",
       fill = "Times used")
ggsave("RotterdamPasOrigin.png")

p02 <- ggplot(dsUserNeiSpat, aes(x = long, y = lat, group = group,
                                 fill = cut(count, c(0, 10, 100, 500, 2000, 10000, Inf), 
                                            labels = c("0-10", "10-100", "100-500", "500-2,000", "2,000-10,000", "10,000+")))) +
  geom_polygon() +
  scale_fill_brewer(palette = "YlOrRd") +
  labs(title = "Origin of the RotterdamPas per Neighborhood over the year",
       fill = "Times used") 

panim2 <- p02+
  transition_time(use_date)  +
  ggtitle("RotterdamPas used by Neighborhoods' inhabitants, for month: {frame_time}") 

gganimate::animate(panim2, renderer = av_renderer())
anim_save("animatedRotterdamPasOrigin.mp4")

######## BOXPLOT TIME


######## DSITRIB ACTIVITIES PER MONTH

dsActivities <- dsRotterdamPas[, c("activity_category", "use_date")]
setDT(dsActivities)
dsActivities[, month := format(dsActivities$use_date, "%B")]
dsActivities$use_date <- NULL
orderMonth <- c("January", "February", "March", "April", "May", "June", 
                "July", "August", "September", "October", "November", "December")


distribActivities <- ggplot(dsActivities, aes(month, fill = activity_category)) +
  geom_bar(position = "fill") + 
  scale_x_discrete(limits = orderMonth) +
  scale_fill_brewer(palette = "YlOrRd") +
  labs(title = "Proportion of Activity Categories over a year", x = "Month", y = "Percent", fill = "Activity Category") +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 45))
ggsave("ActivitiesPerMonth.png")


