### Code to prep CA gridded MTBS data for use in RShiny App
##  Created by Catherine Ledna, 4/30/19

library(rgdal)
library(tidyverse)
library(rgeos)

## Load original MTBS data and California data
mtbs_pts <- readOGR(dsn="../data/mtbs_fod_pts_data/")
mtbs_perim <- readOGR(dsn="../data/mtbs_perimeter_data/")
county_shapefile <- readOGR(dsn = "../data/county_bp_values/")
ca <- county_shapefile[county_shapefile@data$STATEFP=="06",] # For now selecting only CA

plot(mtbs)
#plot(mtbs_perim)

# Reproject MTBS data to CA CRS
mtbs_pts_ca <- spTransform(mtbs_pts, CRS(proj4string(ca)))
mtbs_perim_ca <- spTransform(mtbs_perim, CRS(proj4string(ca)))
mtbs_perim_ca <- mtbs_perim_ca[grepl("CA",mtbs_perim_ca@data$Fire_ID),]

# Subset MTBS data to CA only 
int_pt <- gIntersection(mtbs_pts_ca, ca)

plot(mtbs_perim_ca[mtbs_perim_ca@data$Year==2005,])
plot(ca, add=T)

writeOGR(obj=mtbs_perim_ca, dsn="../data/mtbs_perim_ca/", layer="mtbs_perim_ca", driver="ESRI Shapefile")
writeOGR(obj=mtbs_pts_ca, dsn="../data/mtbs_pts_ca/", layer="mtbs_pts_ca", driver="ESRI Shapefile")
