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
                            selectInput("climate_scenario", "Choose a Climate Scenario:",
                                        choices = c("MPI 4.5 (Warm, Wet)" = "MPI_rcp45", 
                                                    "CCSM4 (Hot, Wet)" = "CCSM4_rcp85", 
                                                    "MIROC 4.5 (Warm,Dry) " = "MIROC_rcp45", 
                                                    "MIROC 8.5 (Hot, Dry)" = "MIROC_rcp85")),
                            # Input: Custom 30 yr periods format with basic animation
                            sliderTextInput("climate_time","Time Periods" , 
                                            choices = c("1981 - 2010", 
                                                        "2010 - 2039", 
                                                        "2040 - 2069", 
                                                        "2070 - 2099"),
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
    
    
    climate_scen <-switch(input$climate_scenario,
                 "MPI 4.5 (Warm, Wet)"=climate_scenario<-"MPI_rcp45", 
                 "CCSM4 (Hot, Wet)"=climate_scenario<-"CCSM4_rcp85", 
                 "MIROC 4.5 (Warm,Dry)"=climate_scenario<-"MIROC_rcp45", 
                 "MIROC 8.5 (Hot, Dry)"=climate_scenario<-"MIROC_rcp85")
    
    climate_time <-switch(input$climate_time,
                 "2010-2039"=climate_time<-"_2010_2039",
                 "2040-2069"=climate_time<-"_2040_2069", 
                 "2070-2099"=climate_time<-"_2070_2099")
    
    climate_var<-switch(input$variable,
                        "Climate Water Deficit"=proj<-"cwd",
                        "Precipitation"=proj<-"ppt", 
                        "Minimum Winter Temperature"=proj<-"tmn", 
                        "Maximum Summer Temperature"=proj<-"tmx")
    
    
    
    
    climate_scr<-raster(paste0("G:/data/GitHub/244_JUAW/app/data/climate/scr/",climate_scen, climate_time,"/",climate_var,".tif")) 
    proj4string(climate_scr) <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
    
    
    climate_sri<-raster(paste0("G:/data/GitHub/244_JUAW/app/data/climate/sri/",climate_scen, climate_time,"/",climate_var,".tif")) 
    proj4string(climate_sri) <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
    
    leaflet() %>% 
      addProviderTiles(providers$Esri.WorldImagery) %>% 
      setView(lng = -120.107103, lat = 33.968757, zoom = 11) %>% 
      addRasterImage(climate_sri, opacity = 0.8)
  
  })
  

    
}
  
  
  

# Run the application 
shinyApp(ui = ui, server = server)

