## RShiny Interactive Map platform for wildfires visualization
# Last updated by Catherine Ledna, 4/16/2019
# Initial code adapted from Shiny gallery: https://github.com/rstudio/shiny-examples/blob/master/063-superzip-example/server.R
# Leaflet Choropleth Tutorial: https://rstudio.github.io/leaflet/choropleths.html 

# Proposed Viz Features: 
#   Choropleth map with county-level interactive polygons (click/hover over county and display value for metric)
#   Sidebar interactive features: 
#     Select a metric for choropleth layer (i.e. burn probability/)
#     Display plot showing how county selected compares to state/country as a whole (hist? scatter?)
#     Additional features/ideas?

library(tidyverse)
library(shiny)
library(leaflet)

# Define variables to be selected (must be in data)
burnvars <- c("Burn Probability" = "bp",
                          "Property Value" = "pv",
                          "WUI Class" = "wCl2019")
wuiYears <- c("1975","1985","1995","2005","2015","2019")
burnYears <- c("2019")

wuivars <- c("Area in WUI (km)" = "area_km", "Area in WUI (percent)" = "area_pct", "Houses in WUI"="housing_units",
             "Houses in WUI (percent)" = "housing_pct", "Population in WUI"="pop", "Population in WUI (percent)"="pop_pct")



navbarPage("Wildfires", id="nav",
           
           tabPanel("Interactive map",
                    div(class="outer",
                        
                      #  tags$head(
                      #    # Include our custom CSS
                      #    includeCSS("styles.css"),
                      #    includeScript("gomap.js")
                      #  ),
                        
                        # Render Leaflet base tiles 
                        # If not using custom CSS, set height of leafletOutput to a number instead of percent
                        # TODO look into custom CSS with just '100%' options to adapt to a variety of browsers
                        leafletOutput("map", width=900, height=650),
                        
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                    draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                    width = 330, height = "auto",align="center",
                                    
                                    h2("Wildfires explorer"),
                                    fixedRow(
                                      column(12,
                                             fixedRow(
                                               column(6,
                                                      selectInput("dataset", "Burn Metric", burnvars, selected=burnvars[1]),
                                                      align="center",offset=1
                                              ),
                                               column(4,
                                                      uiOutput("Year"),
                                                      align="center"
                                                      
                                               ))))
                                    
                                    #selectInput("dataset", "Burn Metric", burnvars, selected=burnvars[1])
                                    # selectInput("size", "Size", vars, selected = "adultpop"),
                                    # conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
                                    #                  # Only prompt for threshold when coloring or sizing by superzip
                                    #                  numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
                                    #)
                                  #  selectInput("wuiMetric","WUI Metrics", wuivars, selected = wuivars[6])
                                  #  plotOutput("wuiPlot", height = 200)
                                #    selectInput("histMetric","Historical Data",histvars, selected=histvars[1]),
                                 #   selectInput("climMetric","Climate Data",climvars, selected=climvars[1])
                                    
                                    
                                  #  plotOutput("scatterCollegeIncome", height = 250)
                        )
                        
                    )
           )
)