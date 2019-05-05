## Catherine Ledna, 5/4/19
## Make Burn Threshold Figure and process data for county-level figs

grid_data <- read_csv("../data/gridded_10km_data.csv")

## Want: Property value in areas w/ bp > 0.01, by WUI Class, at county-level 
##  For all property value years
grid_data_threshold <- grid_data %>% filter(bp > 0.01) %>% 
  dplyr::select(x,y,lon,lat,bp,fips, wuiClass2019, wuiClass2015,wuiClass2005,wuiClass1995,wuiClass1985,wuiClass1975,
         pv2019,pv2015,pv2005,pv1995,pv1985,pv1975,nh2019,nh2015,nh2005,nh1995,nh1985,nh1975)

gd_1975 <- grid_data_threshold %>% dplyr::select(x,y,lon,lat,bp,fips,wuiClass1975,pv1975,nh1975) %>%
  mutate(year=1975)%>% rename(wuiClass=wuiClass1975,pv=pv1975,nh=nh1975)

gd_1985 <- grid_data_threshold %>% dplyr::select(x,y,lon,lat,bp,fips,wuiClass1985,pv1985,nh1985) %>%
  mutate(year=1985)%>% rename(wuiClass=wuiClass1985,pv=pv1985,nh=nh1985)

gd_1995 <- grid_data_threshold %>% dplyr::select(x,y,lon,lat,bp,fips,wuiClass1995,pv1995,nh1995) %>%
  mutate(year=1995)%>% rename(wuiClass=wuiClass1995,pv=pv1995,nh=nh1995)

gd_2005 <- grid_data_threshold %>% dplyr::select(x,y,lon,lat,bp,fips,wuiClass2005,pv2005,nh2005) %>%
  mutate(year=2005)%>% rename(wuiClass=wuiClass2005,pv=pv2005,nh=nh2005)

gd_2015 <- grid_data_threshold %>% dplyr::select(x,y,lon,lat,bp,fips,wuiClass2015,pv2015,nh2015) %>%
  mutate(year=2015)%>% rename(wuiClass=wuiClass2015,pv=pv2015,nh=nh2015)

gd_2019 <- grid_data_threshold %>% dplyr::select(x,y,lon,lat,bp,fips,wuiClass2019,pv2019,nh2019) %>%
  mutate(year=2019)%>% rename(wuiClass=wuiClass2019,pv=pv2019,nh=nh2019)

gd_county <- rbind(gd_1975, gd_1985, gd_1995, gd_2005, gd_2015, gd_2019) %>% group_by(fips,wuiClass,year)%>%
  summarize(nh=sum(nh), pv=sum(pv), bp=mean(bp))

gd_state <- gd_county %>% group_by(wuiClass,year)%>% summarize(nh=sum(nh),pv=sum(pv)) %>% na.omit()

p1 <- ggplot(gd_state)+
  geom_line(aes(year,pv / 10^6,group=wuiClass,color=factor(wuiClass)))+
  theme_bw()+
  ylab("Property Value (Millions)")+
  scale_color_hue(name="WUI Class",labels=wui_cats)+
  ggtitle("Property Value above 1 in 100 Burn Probability by WUI Class, Statewide")

p2 <- ggplot(gd_state)+
  geom_line(aes(year,nh,group=wuiClass,color=factor(wuiClass)))+
  theme_bw()+
  ylab("Number of Houses")+
  scale_color_hue(name="WUI Class",labels=wui_cats)+
  ggtitle("Number of Houses above 1 in 100 Burn Probability by WUI Class, Statewide")

ggsave(p1,file="../data/state_PV_bp_over_1.png",width=7)
ggsave(p2,file="../data/state_NH_bp_over_1.png",width=7 )

write_csv(gd_state,"../data/state_bp_over_1.csv")
write_csv(gd_county, "../data/county_bp_over_1.csv")
