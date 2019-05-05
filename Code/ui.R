## RShiny Interactive Map platform for wildfires visualization
# Last updated by Catherine Ledna, 5/3/2019
# Initial code adapted from Shiny gallery: https://github.com/rstudio/shiny-examples/blob/master/063-superzip-example/server.R
# Leaflet Choropleth Tutorial: https://rstudio.github.io/leaflet/choropleths.html 

library(tidyverse)
library(shiny)
library(leaflet)

# Define variables to be selected (must be in data)

navbarPage("Wildfires", id="nav",
           
           tabPanel("Interactive map",
                    div(class="outer",
                        
                        # Render Leaflet base tiles 
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
                                                      
                                               ))),
                                      column(12,
                                             fixedRow(
                                               column(10,
                                                      selectInput("wuiMetric","WUI Metric", wuivars, selected = wuivars[1]),
                                                      align="center",offset=1

                                                      )
                                               
                                          
                                             ))),
                                    
                                  plotOutput("wuiPlot", height = 200),
                                  sliderInput("historic","Historic Wildfires",min=1984,max=2016,value=c(2010,2016),sep = "")

                        )
                        
                    )
           )
)