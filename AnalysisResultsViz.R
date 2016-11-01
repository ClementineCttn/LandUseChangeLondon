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
mappingVariables = c("PREVIOUS_USE", "CURRENT_USE", "PROPOSED_USE")
years = c(2007, 2010, 2011)
londonBoroughs = c("Barking and Dagenham", "Barnet", "Bexley", "Brent", "Bromley", "Camden", "City of London", 
                   "Croydon", "Ealing", "Enfield", "Greenwich", "Hackney", "Hammersmith and Fulham", "Haringey",
                   "Harrow", "Havering", "Hillingdon", "Hounslow", "Islington", "Kensington and Chelsea", "Kingston upon Thames",
                   "Lambeth", "Lewisham", "Merton", "Newham", "Redbridge", "Richmond upon Thames", "Southwark", "Sutton", 
                   "Tower Hamlets", "Waltham Forest", "Wandsworth", "Westminster")

mapLandUseTransition = function(year, WhatToMap){
  # import file
  if(year == 2007) {
    nlud = read.csv("NLUD_ABCD_2007_LDN.csv", sep=",", dec=".")
    nlud$PREVIOUS_USE = nlud_SP_LL$PREVIOUS_LAND_USE
  } else {
    nlud = read.csv(paste0("NLUD_ABCD_", year, ".csv"), sep=",", dec=".", stringsAsFactors = F)
     if(year %in% c(2011, 2012)) {
      nlud$EASTING = as.numeric(nlud$Easting)
      nlud$NORTHING = as.numeric(nlud$Northing)
      nlud$PREVIOUS_USE = nlud$Previous.use
      nlud$CURRENT_USE = nlud$Current.use
      nlud$PROPOSED_USE = nlud$Proposed.use
      nlud$AREA = as.numeric(nlud$Area)
      nlud$PAO_DESCRIPTION = nlud$PAO.Description

            nlud = nlud[nlud$LA %in% londonBoroughs, ]
     } else {
       nlud = nlud[nlud$REGION == "London", ]
     }
    
  }
  
  # transform data frame into spatialPointsDataFrame and reproject into ukgrid system
  nlud = subset(nlud, EASTING != "" | NORTHING != "" | !is.na(EASTING) | !is.na(NORTHING))
  nlud$nlud_ID <- 1:nrow(nlud)
  coords <- as.data.frame(cbind( Easting = nlud$EASTING,Northing = nlud$NORTHING))
  nlud_SP <-SpatialPointsDataFrame(coords, data = data.frame(nlud), proj4string =CRS(ukgrid))
  nlud_SP_LL <- spTransform(nlud_SP,CRS(latlong))
  colnames (nlud_SP_LL@coords)[colnames(nlud_SP_LL@coords) == "Easting"] <- "Longitude"
  colnames(nlud_SP_LL@coords)[colnames (nlud_SP_LL@coords) == "Northing"] <- "Latitude"
  nlud_SP_LL@data$long = nlud_SP_LL@coords[,1]
  nlud_SP_LL@data$lat = nlud_SP_LL@coords[,2]
  
  # Interactive map
  nlud_SP_LL@data$varToMap = nlud_SP_LL@data[, WhatToMap]
  vals = unique(nlud_SP_LL@data$varToMap)
  n = length(vals)
  lookup = data.frame(plu = vals, plu_id = 1:n)
  nlud_SP_LL@data = data.frame(nlud_SP_LL@data, lookup[match(nlud_SP_LL@data$varToMap, lookup$plu),])
  factpal <- colorFactor(rainbow(n), nlud_SP_LL@data$plu_id)
  vLegendBox <- as.character(vals)
  summary(nlud_SP_LL@data)
  leaflet(data = nlud_SP_LL) %>% addProviderTiles("CartoDB.Positron") %>%
    clearShapes() %>% 
    setView(lng=-0.1855676, lat=51.5371635, zoom=10) %>% 
    addCircleMarkers(~long, ~lat, radius = ~AREA, col=~factpal(plu_id) , 
                     popup = ~paste(PAO_DESCRIPTION, " | Previously: ",PREVIOUS_USE,
                                    " | In ", year, ": ",CURRENT_USE, " | Proposed: ",PROPOSED_USE, sep=" "))
}
mapLandUseTransition(2007, mappingVariables[1])
########## transition 
# transitions_m = table(nlud_SP_LL@data$PREVIOUS_LAND_USE, nlud_SP_LL@data$PROPOSED_USE)
# transitions<-melt(transitions_m)
# names(transitions)=c("previous","current","Fq")
# head(transitions, 4)
# 
# transitions = transitions[transitions$Fq >0, ]




