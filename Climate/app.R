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
library(RColorBrewer)
library(shinyWidgets)
library(leaflet)
library(sf)
library(raster)

test <- raster("G:/data/GitHub/244_JUAW/app/data/climate/sri/CCSM4_rcp85_2010_2039/1.tif")

plot(test)

# Set the directory name where we ill get files from 
climate_dir <- list.dirs("G:/data/GitHub/244_JUAW/app/data/climate/sri")

cwd <- "1.tif"
ppt <- "2.tif"
tmn <- "3.tif"
tmx <- "4.tif"

MPI_rcp45 <- "MPI_rcp45"
CCSM4_rcp85 <- "CCSM4_rcp85"
MIROC_rcp45 <- "MIROC_rcp45"
MIROC_rcp85 <- "MIROC_rcp85"

historic <- "historic"
first <- "2010_2039"
second <-"2040_2069"
third <- "2070_2099"

# Define UI for application that displays data for fog scenarios on SRI and SCR
ui <- navbarPage("EXPLICIT: Sweaty Oak Nuts)", theme = shinytheme("flatly"),
                 
                 # Application title
                 tabPanel("Oak Points"),
                 tabPanel("Fog Scenarios"),
                 tabPanel("Climate Scenarios",
                          
                          sidebarLayout(
                            sidebarPanel(
                            selectInput("variable", "Choose an Environmental Variable:",
                                        choices = c("Climate Water Deficit" = "cwd", 
                                                    "Precipitation" = "2.tif", 
                                                    "Minimum Winter Temperature" = "3.tif", 
                                                    "Maximum Summer Temperature" = "4.tif")),
                            selectInput("scenario", "Choose a Climate Scenario:",
                                        choices = c("MPI 4.5 (Warm, Wet)" = "MPI_rcp45", 
                                                    "CCSM4 (Hot, Wet)" = "CCSM4_rcp85", 
                                                    "MIROC 4.5 (Warm,Dry) " = "MIROC_rcp45", 
                                                    "MIROC 8.5 (Hot, Dry)" = "MIROC_rcp85")),
                            # Input: Custom 30 yr periods format with basic animation
                            sliderTextInput("time","Time Periods" , 
                                            choices = c("1981 - 2010" = "historic", 
                                                        "2010 - 2039" = "first", 
                                                        "2040 - 2069" = "second", 
                                                        "2070 - 2099" = "third"),
                                            animate = TRUE),
                            checkboxInput("climate_legend", "Show legend", TRUE),
                            selectInput("raster_color", "Choose Color:",
                                        c("Blue" = "#004d99",
                                          "Red" = "darkred",
                                          "Green" = "yellowgreen",
                                          "Purple" = "#8a4f7e",
                                          "Orange" = "#d9a440"))
                          ),
          
                          mainPanel(
                            tabsetPanel(
                              tabPanel("Santa Cruz",
                                       leafletOutput("SCRclimatemap")),
                              tabPanel("Santa Rosa",
                                       leafletOutput("SRIclimatemap"))
                            )
                          ),
                          
                          position = c("left", "right"),
                          fluid = FALSE
                          )
                          
                          
                          )
                 
)


server <- function(input, output) {
  
  # Raster Palette Color
  raster_color <- reactive({
    colorNumeric(input$raster_color)
    
  })
  
  title_legend <- reactive({
    input$title_legend
    
  })
  
  # What we need to do here is make it call the correct tiff file
  
  
  
  climate_time <- reactive({
    grep(input$time, climate_dir, value = TRUE)

  })
  
  climate_scenario <- reactive({
    grep(input$scenario, climate_time(), value = TRUE)
    
  })
  
  climate_raster0 <- reactive({
    file.path(climate_scenario(), input$variable)
    
  })
  
  climate_raster <- reactive({
    raster(climate_raster0)
  })
  
  # Calling the Leaflet map
  
  output$SRIclimatemap <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$Esri.WorldImagery) %>% 
      setView(lng = -120.107103, lat = 33.968757, zoom = 11)

  })
  
  observe({
    
    leafletProxy("SRIclimatemap", data = data) %>%
      clearShapes() %>%
      addRasterImage(climate_raster(), colors = raster_color()
      )
  })
  
  observe({
    climate_proxy <- leafletProxy("SRIclimatemap") # Need to make the data refer to the right raster
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    climate_proxy %>% clearControls()
    if (input$climate_legend) {
      pal <- raster_color()
      proxy %>% addLegend(position = "bottomright",
                          pal = raster_color(), values = values(climate_raster()),
                          title = title_legend()
      )
    }
    })
  }
  
  
  

# Run the application 
shinyApp(ui = ui, server = server)

