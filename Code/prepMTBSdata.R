### Code to prep CA gridded MTBS data for use in RShiny App
##  Created by Catherine Ledna, 5/3/19
## TODO: GENERALIZE CODE TO ALL OF USA

library(rgdal)
library(tidyverse)
library(rgeos)

## Load original MTBS data and California data
mtbs_pts <- readOGR(dsn="../data/mtbs_fod_pts_data/")
mtbs_perim <- readOGR(dsn="../data/mtbs_perimeter_data/")
county_shapefile <- readOGR(dsn = "../data/county_bp_values/")
ca <- county_shapefile[county_shapefile@data$STATEFP=="06",] # For now selecting only CA

# Reproject MTBS data to CA CRS and filter to CA only 
mtbs_pts_ca <- spTransform(mtbs_pts, CRS(proj4string(ca)))
mtbs_pts_ca <- mtbs_pts_ca[grepl("CA",mtbs_pts_ca@data$Fire_ID),]
mtbs_perim_ca <- spTransform(mtbs_perim, CRS(proj4string(ca)))
mtbs_perim_ca <- mtbs_perim_ca[grepl("CA",mtbs_perim_ca@data$Fire_ID),]

# Group to FIPS CODE (CA ONLY)
#   TODO: UPDATE THIS MAPPING FOR ALL USA SHAPEFILES
wui_county <- read_csv("../data/wui_class_county.csv") 
fips_names <- as.character(unique(wui_county$fips))
mtbs_pts_ca@data$id <- rownames(mtbs_pts_ca@data)
mtbs_pts_ca@data$fips<- NA
mtbs_pts_ca@data$year <- gsub("/.*","",mtbs_pts_ca@data$Ig_Date)

assignFips <- function(shapefile,mtbsfile,fipscode){
  fips_shp <- shapefile[shapefile@data$COUNTYFP==sub("6","",fipscode),]
  ids <- mtbsfile[fips_shp,]@data$id  
  
  mtbsfile$fips[mtbsfile$id %in% ids]<- fipscode
  return(mtbsfile)
}

for (i in fips_names){
  mtbs_pts_ca <- assignFips(ca, mtbs_pts_ca,i)
  
}

# quick plots to check outputs 
plot(mtbs_pts_ca[is.na(mtbs_pts_ca$fips),])
plot(ca,add=T)
mtbs_pts_ca <- mtbs_pts_ca[!(is.na(mtbs_pts_ca$fips)),]

plot(ca[ca$COUNTYFP=="063",])
plot(mtbs_pts_ca[mtbs_pts_ca@data$fips=="6063",],add=T)

writeOGR(obj=mtbs_perim_ca, dsn="../data/mtbs_perim_ca/", layer="mtbs_perim_ca", driver="ESRI Shapefile")
writeOGR(obj=mtbs_pts_ca, dsn="../data/mtbs_pts_ca/", layer="mtbs_pts_ca", driver="ESRI Shapefile")
