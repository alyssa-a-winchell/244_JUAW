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
library(rgdal)


# Define UI for application that displays data for fog scenarios on SRI and SCR
ui <- navbarPage("EXPLICIT: Sweaty Oak Nuts)", theme = shinytheme("flatly"),
                 
                 # Application title
                 tabPanel("Oak Points"),
                 tabPanel("Fog Scenarios"),
                 tabPanel("Climate Scenarios",
                          
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("climate_variable", "Choose an Environmental Variable:",
                                          choices = c("Climate Water Deficit", 
                                                      "Precipitation", 
                                                      "Minimum Winter Temperature", 
                                                      "Maximum Summer Temperature")),
                              selectInput("climate_scenario", "Choose a Climate Scenario:",
                                          choices = c("MPI 4.5 (Warm, Wet)", 
                                                      "CCSM4 (Hot, Wet)", 
                                                      "MIROC 4.5 (Warm,Dry)", 
                                                      "MIROC 8.5 (Hot, Dry)")),
                              # Input: Custom 30 yr periods format with basic animation
                              sliderTextInput("climate_time","Time Periods", 
                                              choices = c("2010-2039", 
                                                          "2040-2069", 
                                                          "2070-2099"),
                                              animate = TRUE),
                              checkboxInput("climate_legend", "Show legend", TRUE),
                              selectInput("raster_color_climate", "Choose Color Theme:",
                                          c("Rainbow" = "Spectral",
                                            "Yellow, Green, Blue" = "YlGnBu",
                                            "Yellow, Green" = "YlGn",
                                            "Purple, Red" = "PuRd",
                                            "Yellow, Orange, Red" = "YlOrRd"))
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

  # Calling the Leaflet map
  
  
  output$SRIclimatemap <- renderLeaflet({
    
    
    climate_scen <-switch(input$climate_scenario,
                          "MPI 4.5 (Warm, Wet)"=climate_scen<-"MPI_rcp45", 
                          "CCSM4 (Hot, Wet)"=climate_scen<-"CCSM4_rcp85", 
                          "MIROC 4.5 (Warm,Dry)"=climate_scen<-"MIROC_rcp45", 
                          "MIROC 8.5 (Hot, Dry)"=climate_scen<-"MIROC_rcp85")
    
    climate_hands <-switch(input$climate_time,
                          "2010-2039"=climate_hands<-"2010_2039",
                          "2040-2069"=climate_hands<-"2040_2069", 
                          "2070-2099"=climate_hands<-"2070_2099")
    
    climate_var<-switch(input$climate_variable,
                        "Climate Water Deficit"=climate_var<-"cwd",
                        "Precipitation"=climate_var<-"ppt", 
                        "Minimum Winter Temperature"=climate_var<-"tmn", 
                        "Maximum Summer Temperature"=climate_var<-"tmx")
    
  
    
    climate_sri<-raster(paste0("data/climate/sri/", climate_scen,  "_", climate_hands, "/", climate_var, ".tif"))
    # climate_sri<-raster(paste0("data/climate/sri/historic/cwd.tif")) 
    proj4string(climate_sri) <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
    
    
    #This is where you want ot start copying code from
    climate_stack_list <- list.dirs("data/climate/sri/", recursive = TRUE, full.names = TRUE)
    files <- climate_stack_list[grep(paste0(climate_scen), climate_stack_list, fixed=T)]
    climate_files <- dir(files, recursive=TRUE, full.names=TRUE, pattern = paste0(climate_var, ".tif"))
    
    climate_stack <- stack(climate_files)
    
    climate_colors <- reactive({ # delete
      input$raster_color_climate
    })
    
    pal <- colorNumeric( 
      palette = climate_colors(), # pick "Blues"
      domain = values(climate_stack),
      na.color = NA,
      reverse = TRUE
    )
    
     climate_title <- reactive({
       input$climate_variable
    })

     myLabelFormat = function(..., reverse_order = FALSE){ 
       if(reverse_order){ 
         function(type = "numeric", cuts){ 
           cuts <- sort(cuts, decreasing = T)
         } 
       }else{
         labelFormat(...)
       }
     }
    
    leaflet() %>% 
      addProviderTiles(providers$Esri.WorldImagery) %>% 
      setView(lng = -120.107103, lat = 33.968757, zoom = 11) %>% 
      addRasterImage(climate_sri, colors = pal, opacity = 0.8) %>% 
      addLegend("bottomright", pal = pal, values= values(climate_stack),
                title = climate_title(), 
                labFormat = myLabelFormat(reverse_order = T))
    # And stop here 
    
  })
  
  
  
  output$SCRclimatemap <- renderLeaflet({
    
    
    climate_scen <-switch(input$climate_scenario,
                          "MPI 4.5 (Warm, Wet)"=climate_scen<-"MPI_rcp45", 
                          "CCSM4 (Hot, Wet)"=climate_scen<-"CCSM4_rcp85", 
                          "MIROC 4.5 (Warm,Dry)"=climate_scen<-"MIROC_rcp45", 
                          "MIROC 8.5 (Hot, Dry)"=climate_scen<-"MIROC_rcp85")
    
    climate_hands <-switch(input$climate_time,
                           "2010-2039"=climate_hands<-"2010_2039",
                           "2040-2069"=climate_hands<-"2040_2069", 
                           "2070-2099"=climate_hands<-"2070_2099")
    
    climate_var<-switch(input$climate_variable,
                        "Climate Water Deficit"=climate_var<-"cwd",
                        "Precipitation"=climate_var<-"ppt", 
                        "Minimum Winter Temperature"=climate_var<-"tmn", 
                        "Maximum Summer Temperature"=climate_var<-"tmx")
    
    
    
    climate_scr<-raster(paste0("data/climate/scr/", climate_scen,  "_", climate_hands, "/", climate_var, ".tif"))

    proj4string(climate_scr) <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
    
    climate_stack_list <- list.dirs("data/climate/scr/", recursive = TRUE, full.names = TRUE)
    files <- climate_stack_list[grep(paste0(climate_scen), climate_stack_list, fixed=T)]
    climate_files <- dir(files, recursive=TRUE, full.names=TRUE, pattern = paste0(climate_var, ".tif"))
    
    climate_stack <- stack(climate_files)
    
    climate_colors <- reactive({
      input$raster_color_climate
    })
    
    pal <- colorNumeric(
      palette = climate_colors(),
      domain = values(climate_stack),
      na.color = NA,
      reverse = TRUE
    )
    
    climate_title <- reactive({
      input$climate_variable
    })
    
    
    myLabelFormat = function(..., reverse_order = FALSE){ 
      if(reverse_order){ 
        function(type = "numeric", cuts){ 
          cuts <- sort(cuts, decreasing = T)
        } 
      }else{
        labelFormat(...)
      }
    }
    
    
    
    leaflet() %>% 
      addProviderTiles(providers$Esri.WorldImagery) %>% 
      setView(lng = -119.722862, lat = 34.020433, zoom = 11) %>% 
      addRasterImage(climate_scr, colors = pal, opacity = 0.8) %>% 
      addLegend("bottomright", pal = pal, values= values(climate_stack),
                title = climate_title(),
                labFormat = myLabelFormat(reverse_order = T))
    
    
    
  })
  


}



# Run the application 
shinyApp(ui = ui, server = server)

