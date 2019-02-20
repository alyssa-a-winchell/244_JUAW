#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(sf)
library(raster)
library(leaflet)
library(RColorBrewer)


# Read in the data
combo <- read.csv("G:/data/GitHub/244_JUAW/app/data/oaks/sri/combo.csv")
scr_points <- read.csv("G:/data/GitHub/244_JUAW/app/data/oaks/scr/all_4326.csv")

# Make icon for the maps as marker image. However it would work better with circles
leaf <- makeIcon(
  iconUrl = "G:/data/GitHub/244_JUAW/app/data/oaks/leaf_icon.png", 
  iconWidth = 50, iconHeight = 50)




# Define UI for application that displays data for fog scenarios on SRI and SCR
ui <- navbarPage("Oak Nuts ;)", theme = shinytheme("flatly"),
                 
                 # Application title
                 tabPanel("Oak Points",
                          sidebarLayout(
                            sidebarPanel(selectInput("points_colors", "Choose Color:",
                                                     c("Blue" = "#004d99",
                                                       "Red" = "darkred",
                                                       "Green" = "yellowgreen",
                                                       "Purple" = "#8a4f7e",
                                                       "Orange" = "#d9a440"))
                            ),
                            mainPanel(
                              
                              tabsetPanel(
                                tabPanel("Santa Cruz",
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
                          
                          
                          
                          
                          
                          
                 )
                 ),
                 
                 
                 tabPanel("Fog"),
                 tabPanel("Climate Scenarios")
                 
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  points_color <- reactive({
    input$points_colors
    
  })
  
  output$SCRpoints <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$Esri.WorldImagery) %>% 
      setView(lng = -119.722862, lat = 34.020433, zoom = 11) %>% 
      addCircleMarkers(radius = 4, fillColor = points_color(), stroke = FALSE, fillOpacity = 0.5, data = scr_points, lng = ~POINT_X, lat = ~POINT_Y)
    
  })
  
  filteredData <- reactive({
    combo[ combo$Age == input$age, ]
  })
  
  sri_color <- reactive({
    input$points_colors
    
  })
  
  output$SRIpoints <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$Esri.WorldImagery) %>% 
      setView(lng = -120.107103, lat = 33.968757, zoom = 11) 
    
  })
  
  
  observe({
    
    leafletProxy("SRIpoints") %>%
      clearMarkers() %>% 
      addCircleMarkers(data = filteredData(), lng = ~POINT_X, lat = ~POINT_Y, radius = 4, fillColor = points_color(), stroke = FALSE, fillOpacity = 0.4)
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

