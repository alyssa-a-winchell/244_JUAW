#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(shinythemes)
library(sf)
library(raster)
library(leaflet)

combo <- read.csv("app/data/oaks/sri/combo.csv")
scr_points <- read.csv("app/data/oaks/scr/all_4326.csv")


leaf <- makeIcon(
  iconUrl = "app/data/oaks/leaf_icon.png", 
  iconWidth = 50, iconHeight = 50)




# Define UI for application that displays data for fog scenarios on SRI and SCR
ui <- navbarPage("Oak Nuts ;)", theme = shinytheme("flatly"),
                 
                 # Application title
                 tabPanel("Oak Points",
                          
                          mainPanel(
                            tabsetPanel(
                              tabPanel("Santa Cruz",
                                       selectInput("points_colors", "Choose Color:",
                                                   c("Blue" = "blue",
                                                     "Red" = "red",
                                                     "Green" = "green",
                                                     "Purple" = "purple",
                                                     "Yellow" = "yellow",
                                                     "Orange" = "orange")),
                                       
                                       leafletOutput("SCRpoints")),
                              tabPanel("Santa Rosa",
                                       sidebarPanel(
                                         radioButtons("age", "Choose Age Group:",
                                                      c("Seedlings" = "seed",
                                                        "Adults" = "adult",
                                                        "All" = "all")),
                                         width = 5
                                       ),
                                       leafletOutput("SRIpoints")
                                       
                              )
                            )
                          )
                          
                          
                          
                 ),
                 
                 
                 tabPanel("Fog"),
                 tabPanel("Climate Scenarios")
                 
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$SCRpoints <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      setView(lng = -119.722862, lat = 34.020433, zoom = 11) %>% 
      addMarkers(data = scr_points, lng = ~POINT_X, lat = ~POINT_Y, icon = leaf)
    
  })
  
  filteredData <- reactive({
    combo[ combo$Age == input$age, ]
  })
  
  output$SRIpoints <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$Esri.WorldStreetMap) %>% 
      setView(lng = -120.107103, lat = 33.968757, zoom = 11) 
    
  })
  
  
  observe({
    
    leafletProxy("SRIpoints") %>%
      clearMarkers() %>% 
      addMarkers(data = filteredData(), lng = ~POINT_X, lat = ~POINT_Y, icon = leaf)
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

