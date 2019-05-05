# Author: Catherine Ledna; date 4/30/19
#   Script to load all necessary data and variable definitions to run rshiny app 
#   
#------------------------------------------------------------------------------------------------------------------
### Step 1: Load Data Sources: County Shapefiles, Gridded Data, and Historic Burn Shapefiles and Points
#------------------------------------------------------------------------------------------------------------------
#County shapefiles:
county_shapefile <- readOGR(dsn = "../data/county_bp_values/")
ca <- county_shapefile[county_shapefile@data$STATEFP=="06",] # For now selecting only CA to plot bc it's faster
CA_ONLY <- T

# Gridded Property Value, Burn Probability, and WUI Data
if(CA_ONLY==T){
  grid <- readOGR(dsn="../data/grid_ca/") 
}

# MTBS Data: points and burn probability for historic wildfire data 
if (CA_ONLY==T){ # Load data subset to CA 
  mtbs_perim <- readOGR(dsn="../data/mtbs_perim_ca/")
  mtbs_pts <- readOGR(dsn="../data/mtbs_pts_ca/")
}

if(CA_ONLY==T) inputdata<- ca else inputdata <- county_shapefile

#------------------------------------------------------------------------------------------------------------------
# Step 2: Load WUI Data CSV
#------------------------------------------------------------------------------------------------------------------

# WUI and PV by County Stats
# TO DO FIX UP SCRIPT FOR THIS
wui_county <- read_csv("../data/wui_class_county.csv")
wui_over1 <- read_csv("../data/county_bp_over_1.csv")
#------------------------------------------------------------------------------------------------------------------
# Step 3: Define Variables to be Used in App
#------------------------------------------------------------------------------------------------------------------
# Dictionary defining WUI class categories 
wui_Dict <- list(
  "1" = "1 house/40 ac",
  "2" = "1 house/40 - 1 house/20 ac",
  "3" = "1 house/20 - 1 house/10 ac",
  "4" = "1 house/10 - 1 house/5 ac",
  "5" = "1 house/5 - 1 house/2 ac",
  "6" = "1 house/2 - 3 house/ac",
  "7" = "> 3 house/ac"
)
# In character form
wui_cats <- c("1 house/40 ac","1 house/40 - 1 house/20 ac","1 house/20 - 1 house/10 ac",
              "1 house/10 - 1 house/5 ac","1 house/5 - 1 house/2 ac","1 house/2 - 3 house/ac",
              "> 3 house/ac")

# Variable Definitions
# burnvars: corresponds to data in the "grid" data set (10 km resolution)
#   names = what is displayed in drop-down panels in user interface
#   values = corresponding name in the actual dataset 
burnvars <- c("Burn Probability" = "bp",
              "Property Value" = "pv",
              "Expected Burn Damages"="bv_",
              "WUI Class" = "wCl2019") 

# wuiYears: years available for gridded WUI data and property value data
wuiYears <- c("1975","1985","1995","2005","2015","2019")
# burnYears: years available for gridded burn probability 
burnYears <- c("2019")

# wuivars: Variables available to display for WUI data 
wuivars <- c("Property Value by WUI Class"="pv","Residences by WUI Class"="nh")

# Flag to indicate to other apps that this script has been run
LOADED <- T