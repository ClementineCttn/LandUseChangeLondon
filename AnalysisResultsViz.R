##########################################
# Project Land Use change in London
# Clementine Cottineau 
# UCL - CASA - UDL
# 21 October 2016
##########################################

require(sp)
require(rgdal)
require(leaflet)
require(rgeos)
require(raster)
require(maptools)
setwd("/Users/clementinecottineau/Documents/LandUseChangeLondon/data/")

##########################################
# Source of data
# National Land Use Database of Previously Developed Land (NLUD-PDL)
# https://www.gov.uk/government/collections/national-land-use-database-of-previously-developed-land-nlud-pdl
# http://tna.europarchive.org/20081209183550/http://www.nlud.org.uk/nlud/nlud_default.asp
##########################################

ukgrid = "+init=epsg:27700"
latlong = "+init=epsg:4326"

# import file
nlud = read.csv("NLUD_ABCD_2007_LDN.csv", sep=",", dec=".")

# transform data frame into spatialPointsDataFrame and reproject into ukgrid system
nlud = subset(nlud, EASTING != "" | NORTHING != "" | !is.na(EASTING) | !is.na(NORTHING))
nlud$nlud_ID <- 1:nrow(nlud)
coords <- cbind( Easting = nlud$EASTING,Northing = nlud$NORTHING)
nlud_SP <-SpatialPointsDataFrame(coords, data = data.frame(nlud), proj4string =CRS(ukgrid))
nlud_SP_LL <- spTransform(nlud_SP,CRS(latlong))
colnames (nlud_SP_LL@coords)[colnames(nlud_SP_LL@coords) == "Easting"] <- "Longitude"
colnames(nlud_SP_LL@coords)[colnames (nlud_SP_LL@coords) == "Northing"] <- "Latitude"
nlud_SP_LL@data$long = nlud_SP_LL@coords[,1]
nlud_SP_LL@data$lat = nlud_SP_LL@coords[,2]

# Interactive map
nlud_SP_LL@data$varToMap = nlud_SP_LL@data$PREVIOUS_LAND_USE
vals = unique(nlud_SP_LL@data$varToMap)
n = length(vals)
lookup = data.frame(plu = vals, plu_id = 1:n)
nlud_SP_LL@data = data.frame(nlud_SP_LL@data, lookup[match(nlud_SP_LL@data$varToMap, lookup$plu),])
factpal <- colorFactor(rainbow(n), nlud_SP_LL@data$plu_id)
vLegendBox <- as.character(vals)
leaflet(data = nlud_SP_LL) %>% addProviderTiles("CartoDB.Positron") %>%
  clearShapes() %>% 
  setView(lng=-0.1855676, lat=51.5371635, zoom=10) %>% 
  addCircleMarkers(~long, ~lat, radius = ~ AREA, col=~factpal(plu_id) , 
                   popup = ~paste(PAO_DESCRIPTION, " | Previously: ",PREVIOUS_LAND_USE, " | Proposed: ",PROPOSED_USE, sep=" "))



