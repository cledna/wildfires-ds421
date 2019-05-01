# Author: Catherine Ledna; date 4/30/19

### Step 1: Load Data Sources (Outside of App)
#County shapefiles:
county_shapefile <- readOGR(dsn = "../data/county_bp_values/")# Change to point to shapefile location
ca <- county_shapefile[county_shapefile@data$STATEFP=="06",] # For now selecting only CA to plot bc it's faster
CA_ONLY <- T

# Gridded Property Value, Burn Probability, and WUI Data
grid <- readOGR(dsn="../data/grid_ca/")

# MTBS Data: points and burn probability
mtbs_perim <- readOGR(dsn="../data/mtbs_perim_ca/")
mtbs_pts <- readOGR(dsn="../data/mtbs_pts_ca/")

if(CA_ONLY==T) inputdata<- ca else inputdata <- county_shapefile

LOADED <- T