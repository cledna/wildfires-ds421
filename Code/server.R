## RShiny Interactive Map platform for wildfires visualization
# Last updated by Catherine Ledna, 4/16/2019
# Initial code adapted from Shiny gallery: https://github.com/rstudio/shiny-examples/blob/master/063-superzip-example/server.R
# Leaflet Choropleth Tutorial: https://rstudio.github.io/leaflet/choropleths.html 

# Proposed Viz Features: 
#   Choropleth map with county-level interactive polygons (click/hover over county and display value for metric)
#     NOTE: current data format has each metric as a variable in the underlying SpatialPolygonsDataFrame (from processing via Kelly's script)
#     To Do:
#     2. Formatting: 
#       a. Line thickness for counties
#       b. Need a polygons layer for state boundary lines only 
#       c. Other 
#   Sidebar interactive features: 
#     Select a metric for choropleth layer -- DONE
#       TODO: add a legend title and units for each metric 
#     FIRE FACTS!
#       
#     Display plot showing how county selected compares to state/country as a whole (hist? scatter?)
#     Additional features/ideas?

# Required packages
library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(tidyverse)
library(reshape2)
library(rgdal)

# Outside of the app, reading in shapefile data as SpatialPolygonsDataFrame and filtering just to California 
county_shapefile <- readOGR(dsn = "../data/county_bp_values/")# Change to point to shapefile location
ca <- county_shapefile[county_shapefile@data$STATEFP=="06",] # For now selecting only CA to plot bc it's faster
CA_ONLY <- T

if(CA_ONLY==T) inputdata<- ca else inputdata <- county_shapefile


# Read in WUI data and process 
# To do move the processing step to other script 
wui <- read_csv("../data/combined_wui_data.csv") %>%select(FIPS,COUNTY,STATE,var,TotalWUI1990,TotalWUI2000,TotalWUI2010)%>% 
  melt(id.vars=c("FIPS","COUNTY","STATE","var")) %>% mutate(variable=as.character(variable), year=as.numeric(gsub("TotalWUI","",variable)),
                                                            variable="TotalWUI")
  
wuivars <- c("Area in WUI (km)" = "area_km", "Area in WUI (percent)" = "area_pct", "Houses in WUI"="housing_units",
             "Houses in WUI (percent)" = "housing_pct", "Population in WUI"="pop", "Population in WUI (percent)"="pop_pct")

burnvars <- c("Property Value x Burn Probability" = "prop_bp",
              "Property Value x P(Flame Height > 4m)" = "prop_bpg4")




# These are the breaks for the legend / color scale for the default variable 
bins <- c(0, 1e6, 1e7, 1e8, 1e9, 3e9)
pal <- colorBin("YlOrRd", domain = inputdata$prop_value, bins = bins)


function(input, output, session) {
  
  ## Reactive Expression for Variable Selection 
    
 
  ## Render the Leaflet map 
  
  # Create the map
  # Add burn probability tiles 
  output$map <- renderLeaflet({
    leaflet(inputdata)%>%
      setView(-96, 37.8, 4) %>%
      addTiles() %>% # This uses OpenStreetMap as a default provider
      addPolygons(
        layerId= inputdata$COUNTYFP,
        fillColor = ~pal(inputdata[[burnvars[1]]]),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE)
      ) %>%
      addLegend(pal = pal, values = ~prop_value, opacity = 0.7, title = NULL,
                position = "bottomright")
  })
  
  #This will allow the map to react to input data
  observe({
  
    var <- input$dataset # The input selected in drop-down window for dataset 
    data <- inputdata[[var]]
    
    # Recalculate bins based on data ranges 
    calc_bins <- function(vals){
      #nbins <- 6 
      out <- signif(c(min(vals,na.rm=T),quantile(vals,.25), quantile(vals,.5), quantile(vals,.75),max(vals,na.rm = T)), 3)
    }
    
   pal <- colorBin("YlOrRd", domain = data, bins = calc_bins(data))
   
   leafletProxy("map", data = inputdata) %>%
      clearShapes() %>%
      clearControls()%>%
      addPolygons(
        layerId= inputdata$COUNTYFP,
        fillColor = pal(inputdata[[input$dataset]]),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE)
      ) %>%
      addLegend(pal = pal, values = inputdata[[input$dataset]], opacity = 0.7, title = NULL,
                position = "bottomright")

  })
  
  # # Function to display pop-up data 
  showMapPopup <- function(id,lat,lng) {
    selectedCounty <- inputdata[inputdata$COUNTYFP == id,]
    countyStateFP <- as.numeric(paste0(selectedCounty$STATEFP[1],selectedCounty$COUNTYFP[1]))
    
    wuidata <- wui %>% filter(FIPS==countyStateFP, year==2010)
    
     content <- as.character(tagList(
       tags$h4("County:", as.character(selectedCounty$NAME[1])),tags$br(),
       sprintf("Property Value in County (Million $): %s",signif(selectedCounty[["prop_value"]]/10^6,3)),tags$br(),
       sprintf("Housing in WUI (2010): %s%%", wuidata[wuidata$var=="housing_pct",]$value)
      ))

    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = id)
  }

  # Allow pop-up to appear when county is clicked 
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()

    isolate({
      showMapPopup(event$id, event$lat, event$lng)
    })
  })

  selected <- reactive({
    if (is.null(input$map_shape_click))
      return(NULL)
    
    county <- input$map_shape_click
    countyDF <- inputdata[inputdata$COUNTYFP == county$id,]
    countyStateFP <- as.numeric(paste0(countyDF$STATEFP[1],countyDF$COUNTYFP[1]))
    return(countyStateFP)

  })

  output$wuiPlot <- renderPlot({
    # If no county selected, no plot
    if (is.null(selected()))
      return(FALSE)

    wuidata2 <- wui %>% filter(FIPS==selected())

    ggplot(wuidata2 %>% filter(var==input$wuiMetric))+
      geom_line(aes(year,value))+
      ggtitle("Change in WUI, 1990-2010")+
      xlab("")+
      ylab(names(wuivars[wuivars==input$wuiMetric]))
  })
  # 

}
  
  