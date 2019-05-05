## RShiny Interactive Map platform for wildfires visualization
# Last updated by Catherine Ledna, 5/3/2019
# Initial code adapted from Shiny gallery: https://github.com/rstudio/shiny-examples/blob/master/063-superzip-example/server.R
# Leaflet Choropleth Tutorial: https://rstudio.github.io/leaflet/choropleths.html 

# Required packages
library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(tidyverse)
library(reshape2)
library(rgdal)
#setwd("~/Desktop/wildfires-clean/Code") # To do - change this; currently works if setwd to source file location

if (!exists("LOADED")){
  source("loadData.R")  # Load datasets into workspace
}

#### Step 2: Define Relevant Variables 
burnvars <- c("Burn Probability" = "bp",
              "Property Value" = "pv",
              "WUI Class" = "wCl2019")

function(input, output, session) {
  
  # Define default bins for burn probability
  bp_bins <- c(0,0.003, 0.008, 0.05,0.105) #  TODO thresholds
  pal <- colorBin("YlOrRd", domain = grid$bp, bins = bp_bins)
 
  ## Render the Leaflet map 
  output$map <- renderLeaflet({
    leaflet()%>%
      setView(-116, 37, 5.5) #%>%
    #   addTiles() %>% # This uses OpenStreetMap as a default provider
    #   addPolygons( # Add burn probability tiles 
    #     data=grid,
    #     layerId = grid$id,
    #     fillColor = ~pal(bp),
    #     weight=1,
    #     color="transparent",
    #     fillOpacity=0.7
    #   ) %>%
    # addPolygons( # Add county outlines 
    #   data=inputdata,
    #   layerId= inputdata$COUNTYFP,
    #   fillColor = "transparent",
    #   weight = 2,
    #   opacity = 1,
    #   color = "black",
    #   dashArray = "3",
    #   fillOpacity = 0.7,
    #   highlight = highlightOptions(
    #     weight = 5,
    #     color = "#666",
    #     dashArray = "",
    #     fillOpacity = 0.7,
    #     bringToFront = TRUE)
    # ) %>%
    #   addLegend(data=grid,pal = pal, values = ~bp, opacity = 0.7, title = NULL,
    #             position = "bottomleft")
  })
  
  # Render Year based on dataset selected 
  output$Year <- renderUI({
    if (input$dataset==burnvars[1]){
      yr <- burnYears
      sel <- 1
    }
    else{
      yr <- wuiYears
      sel <- 6
    }
      
    selectInput("Year", "Year", yr,selected=yr[sel])
  })
  
  # Filtered MTBS Data 
  filteredMTBSpts<- reactive({
    mtbs_pts_ca[mtbs_pts_ca$year >= input$historic[1] & mtbs_pts_ca$year <= input$historic[2],]
  })
  
  var <- reactive({
    input$dataset
  })
  yr <- reactive({
    input$Year
  })
  
  #This will allow the map to react to input data
  observe({
   # Recalculate bins based on data ranges
    calc_bins <- function(vals){
      #nbins <- 6
      out <- signif(unique(c(min(vals,na.rm=T),quantile(vals,.25,na.rm=T), quantile(vals,.5,na.rm=T), quantile(vals,.75,na.rm=T),quantile(vals,.9,na.rm=T),max(vals,na.rm = T))), 3)
    }
    
    if (var()=="bv_"){
      var2 <- paste0(var(),input$Year)
      pal <- colorBin("YlOrRd",domain=grid[[var2]],bins=c(0,100000,1000000,100000000,2e9))
      
    }else if (grepl("wCl",var())){
      yr <- input$Year
      var2 <- paste0("wCl",yr())
      pal <- colorFactor("YlOrRd", grid[[var2]])
      
    }else if (var()=="bp"){
      pal <- colorBin("YlOrRd", domain = grid[[var()]], bins = bp_bins)
      var2<- var()
      
    } else if(grepl("pv",var())){
      var2 <- paste0("pv",yr())
      pal <- colorBin("YlOrRd", domain = grid[[var2]], bins = calc_bins(grid[["pv2019"]]))
      
    }else{
      pal <- colorBin("YlOrRd", domain = grid[[var()]], bins = calc_bins(grid[[var()]]))
      var2 <- var()
    }
    
    df <- grid@data[[var2]]
    

   leafletProxy("map") %>%
      removeShape(layerId=grid$id) %>%
     removeShape(layerId=mtbs_pts_ca$id) %>%
      clearControls()%>%
     addPolygons(
       data=grid,
       fillColor = ~pal(df),
       weight=1,
       color="transparent",
       fillOpacity=0.7
     ) %>%
     addPolygons(
       data=inputdata,
       layerId= inputdata$COUNTYFP,
       fillColor = "transparent",
       weight = 2,
       opacity = 1,
       color = "black",
       dashArray = "3",
       fillOpacity = 0.7,
       highlight = highlightOptions(
         weight = 5,
         color = "#666",
         dashArray = "",
         fillOpacity = 0.7,
         bringToFront = TRUE)
     ) %>%
    addCircleMarkers(data=filteredMTBSpts(),radius=2,color="black",fillOpacity = 0.7,
                          layerId = filteredMTBSpts()$id)%>%
     addLegend(pal = pal, values = df, opacity = 0.7, title = NULL,
               position = "bottomleft")

  })

  # # Function to display pop-up data
  showMapPopup <- function(id,lat,lng) {
    selectedCounty <- inputdata[inputdata$COUNTYFP == id,]
    countyStateFP <- as.numeric(paste0(selectedCounty$STATEFP[1],selectedCounty$COUNTYFP[1]))

    #wuidata <- wui %>% filter(FIPS==countyStateFP, year==2010)

     content <- as.character(tagList(
       tags$h4(as.character(selectedCounty$NAME[1])," County")#,tags$br(),
       #sprintf("Property Value in County (Million $): %s",signif(selectedCounty[["prop_value"]]/10^6,3)),tags$br()
       #sprintf("Housing in WUI (2010): %s%%", wuidata[wuidata$var=="housing_pct",]$value)
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

  # Holder for county variable that's clicked
  selected <- reactive({
    if (is.null(input$map_shape_click))
      return(NULL)

    county <- input$map_shape_click
    countyDF <- inputdata[inputdata$COUNTYFP == county$id,]
    countyStateFP <- as.numeric(paste0(countyDF$STATEFP[1],countyDF$COUNTYFP[1]))
    return(countyStateFP)

  })
   
  # Reactive Data frame for county-level wui data over 1 in 100 burn prob 
  wuidata <- reactive({
    wui_over1 %>% filter(fips==selected())
  })
  
  # WUI Plot 
   output$wuiPlot <- renderPlot({
      # If no county selected, no plot
      if (is.null(selected())||nrow(wuidata())==0)
        return(FALSE)
      
      if(input$wuiMetric=="pv"){
        title<- "Property Value over 1 in 100 Burn Probability"
        yl<- "Property value"
        
        p <- ggplot(wuidata())+
          geom_line(aes(year,y=pv,group=wuiClass,color=factor(wuiClass)))+
          theme_bw()+
          ggtitle(title)+
          xlab("")+
          ylab(yl)+
          scale_color_hue("WUI Class",labels=wui_cats[unique(wuidata()$wuiClass)])
        
      } 
      else{
        title <- "Number of houses over 1 in 100 Burn Probability"
        yl <- "Number of Houses"
        
        p <- ggplot(wuidata())+
          geom_line(aes(year,y=nh,group=wuiClass,color=factor(wuiClass)))+
          theme_bw()+
          ggtitle(title)+
          xlab("")+
          ylab(yl)+
          scale_color_hue("WUI Class",labels=wui_cats[unique(wuidata()$wuiClass)])
        
      }
      
      return(p)       
        
   })
   

}
  
