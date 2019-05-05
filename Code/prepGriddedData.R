### Code to prep CA gridded WUI data for use in RShiny App
##  Created by Catherine Ledna, 4/30/19

library(rgdal)
library(tidyverse)
library(rgeos)

## Load original gridded data and California data
grid <- readOGR(dsn="../data/grid_cell/")
county_shapefile <- readOGR(dsn = "../data/county_bp_values/")
ca <- county_shapefile[county_shapefile@data$STATEFP=="06",] # For now selecting only CA

## Reproject gridded data to CA grid 
proj4string(grid)<- '+units=m +init=epsg:5070'
grid_ca <- spTransform(grid, CRS(proj4string(ca)))

# Find intersection 
int <- gIntersection(grid_ca, ca)
g2 <- over(grid_ca,int)
grid_ca <- grid_ca[!(is.na(g2)),]

# Clip to CA lines 
clip2 <- gIntersection(int, grid_ca, byid = TRUE, drop_lower_td = TRUE)
grid_ca@polygons <- clip2@polygons

# Make plot to check aggregation: 
library(RColorBrewer)
my_colors = brewer.pal(9, "Reds") 
my_colors = colorRampPalette(my_colors)(30)

# Attribute on shade to each bp category
class_of_bp= cut(grid_ca@data$bp, 30)
my_colors=my_colors[as.numeric(class_of_bp)]

plot(grid_ca, col=my_colors,border=NA)
plot(ca, add=T)


# Some more data manip
# Rename variables, as naming got messed up in an earlier iteration 
years <- c("2019","2015","2005","1995","1985","1975")
types <- c("p","n") # p = property value, n = number of houses 
classes <- 1:7 # WUI classes 
name_vec <- vector(length=length(years)*length(types)*length(classes))

i=1
for (y in years){
  for(t in types){
    for(c in classes){
      nv <- paste0("b",y,t,c)
      name_vec[i]<- nv
      i=i+1
    }
  }
}

names_old <- names(grid_ca@data)[grepl("built",names(grid_ca@data))]
names(names_old) <- name_vec
names_old <- list(names_old)

# Rename all data 
grid_ca@data <- grid_ca@data %>% rename_(.dots = names_old[[1]])

# Create WUI Class Variables by Grid Cell for years considered: 
#   2019, 2015, 2005, 1995, 1985, 1975
# This is atrocious code, I know. 
grid_ca@data <- grid_ca@data %>% 
  mutate(wuiCl2019 = ifelse(!(is.na(b2019p1)),1,ifelse(!(is.na(b2019p2)),2,
                                                            ifelse(!(is.na(b2019p3)),3,
                                                                   ifelse(!(is.na(b2019p4)),4,
                                                                          ifelse(!(is.na(b2019p5)),5,
                                                                                 ifelse(!(is.na(b2019p6)),6,
                                                                                        ifelse(!(is.na(b2019p7)),7,NA))))))))
grid_ca@data <- grid_ca@data %>% 
  mutate(wuiCl2015 = ifelse(!(is.na(b2015p1)),1,ifelse(!(is.na(b2015p2)),2,
                                                            ifelse(!(is.na(b2015p3)),3,
                                                                   ifelse(!(is.na(b2015p4)),4,
                                                                          ifelse(!(is.na(b2015p5)),5,
                                                                                 ifelse(!(is.na(b2015p6)),6,
                                                                                        ifelse(!(is.na(b2015p7)),7,NA))))))))

grid_ca@data <- grid_ca@data %>% 
  mutate(wuiCl2005 = ifelse(!(is.na(b2005p1)),1,ifelse(!(is.na(b2005p2)),2,
                                                            ifelse(!(is.na(b2005p3)),3,
                                                                   ifelse(!(is.na(b2005p4)),4,
                                                                          ifelse(!(is.na(b2005p5)),5,
                                                                                 ifelse(!(is.na(b2005p6)),6,
                                                                                        ifelse(!(is.na(b2005p7)),7,NA))))))))

grid_ca@data <- grid_ca@data %>% 
  mutate(wuiCl1995 = ifelse(!(is.na(b1995p1)),1,ifelse(!(is.na(b1995p2)),2,
                                                            ifelse(!(is.na(b1995p3)),3,
                                                                   ifelse(!(is.na(b1995p4)),4,
                                                                          ifelse(!(is.na(b1995p5)),5,
                                                                                 ifelse(!(is.na(b1995p6)),6,
                                                                                        ifelse(!(is.na(b1995p7)),7,NA))))))))


grid_ca@data <- grid_ca@data %>% 
  mutate(wuiCl1985 = ifelse(!(is.na(b1985p1)),1,ifelse(!(is.na(b1985p2)),2,
                                                            ifelse(!(is.na(b1985p3)),3,
                                                                   ifelse(!(is.na(b1985p4)),4,
                                                                          ifelse(!(is.na(b1985p5)),5,
                                                                                 ifelse(!(is.na(b1985p6)),6,
                                                                                        ifelse(!(is.na(b1985p7)),7,NA))))))))

grid_ca@data <- grid_ca@data %>% 
  mutate(wuiCl1975 = ifelse(!(is.na(b1985p1)),1,ifelse(!(is.na(b1975p2)),2,
                                                            ifelse(!(is.na(b1975p3)),3,
                                                                   ifelse(!(is.na(b1975p4)),4,
                                                                          ifelse(!(is.na(b1975p5)),5,
                                                                                 ifelse(!(is.na(b1975p6)),6,
                                                                                        ifelse(!(is.na(b1975p7)),7,NA))))))))

grid_ca@data <- grid_ca@data %>% group_by(x,y) %>% mutate(pv2019 = sum(b2019p1,b2019p2,b2019p3,b2019p4,b2019p5,b2019p6,b2019p7,na.rm = T),
                                        nh2019 = sum(b2019n1,b2019n2,b2019n3,b2019n4,b2019n5,b2019n6,b2019n7,na.rm=T),
                                        pv2015 = sum(b2015p1,b2015p2,b2015p3,b2015p4,b2015p5,b2015p6,b2015p7,na.rm = T),
                                        nh2015 = sum(b2015n1,b2015n2,b2015n3,b2015n4,b2015n5,b2015n6,b2015n7,na.rm=T),
                                        pv2005 = sum(b2005p1,b2005p2,b2005p3,b2005p4,b2005p5,b2005p6,b2005p7,na.rm = T),
                                        nh2005 = sum(b2005n1,b2005n2,b2005n3,b2005n4,b2005n5,b2005n6,b2005n7,na.rm=T),
                                        pv1995 = sum(b1995p1,b1995p2,b1995p3,b1995p4,b1995p5,b1995p6,b1995p7,na.rm = T),
                                        nh1995 = sum(b1995n1,b1995n2,b1995n3,b1995n4,b1995n5,b1995n6,b1995n7,na.rm=T),
                                        pv1985 = sum(b1985p1,b1985p2,b1985p3,b1985p4,b1985p5,b1985p6,b1985p7,na.rm = T),
                                        nh1985 = sum(b1985n1,b1985n2,b1985n3,b1985n4,b1985n5,b1985n6,b1985n7,na.rm=T),
                                        pv1975 = sum(b1975p1,b1975p2,b1975p3,b1975p4,b1975p5,b1975p6,b1975p7,na.rm = T),
                                        nh1975 = sum(b1975n1,b1975n2,b1975n3,b1975n4,b1975n5,b1975n6,b1975n7,na.rm=T))

# Create ID variable
grid_ca@data$id <- rownames(grid_ca@data)

# Create burn probability x proprty value variables 
grid_ca@data <- grid_ca@data %>% group_by(x,y)%>% mutate(bv_2019 = pv2019 * bp,
                                                   bv_2015 = pv2015*bp,
                                                   bv_2005 = pv2005*bp,
                                                   bv_1995 = pv1995*bp,
                                                   bv_1985 = pv1985*bp,
                                                   bv_1975 = pv1975*bp)

# Save data 
# Reduce data to 5 sigfigs
grid_ca@data <- grid_ca@data %>% mutate_at(vars(-x,-y,-lon,-lat,-bp,-cfl,-prob_g4,-bp_x_cfl,-bp_x_g4,-id), funs(signif(.,5)))
writeOGR(obj=grid_ca, dsn="../data/grid_ca/", layer="grid_ca", driver="ESRI Shapefile")
write_csv(grid_ca@data,"../data/gridded_10km_data.csv")

## Extract WUI Data at County Level 
wuidata <- grid_ca@data %>% ungroup()%>% dplyr::select(-x,-y,-lon,-lat,-bp,-cfl,-prob_g4, -bp_x_cfl, -bp_x_g4,-id) %>% unique()%>%
  group_by(fips) %>% summarize_at(vars(-fips),funs(sum(.,na.rm=T))) %>% ungroup() %>%
  melt(id.vars=c("fips")) %>% mutate(variable=as.character(variable)) %>%
  filter(!(grepl("wCl",variable)))%>%
  mutate(wuiClass = ifelse(grepl("b1|b2",variable),str_sub(variable,-1),"total"),
         year=ifelse(grepl("wCl|pv|nh",variable),str_sub(variable,-4),gsub("n.*","",gsub("p.*","",str_sub(variable,-6)))),
         variable=ifelse(grepl("b[0-9]{4}p[1-7]",variable),"property value", 
                         ifelse(grepl("b[0-9]{4}n[1-7]",variable),"num houses",
                                ifelse(grepl("pv",variable),"property value",
                                             ifelse(grepl("nh",variable),"num houses",NA))))) %>%
  unique() %>% dcast(fips+year+wuiClass~variable)

wuidata <- wuidata %>%
  melt(id.vars=c("fips","year","wuiClass")) %>% mutate(variable=as.character(variable)) %>% filter(variable!="NA")%>%
  na.omit()

write_csv(wuidata,"../data/wui_class_county.csv")
