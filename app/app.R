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
library(wesanderson)


# Define UI for application that displays data for fog scenarios on SRI and SCR
ui <- navbarPage("EXPLICIT: Sweaty Oak Nuts)", theme = shinytheme("flatly"),
                 
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
                                         h6("Each point symbolizes either individual trees or a grove of island oak. Notice how the oaks in Santa Cruz are concentrated on the northern side of the island. In total, there are 271 total oak points on Santa Cruz."),
                                         leafletOutput("SCRpoints")),
                                
                                tabPanel("Santa Rosa",
                                         h6("Each point symbolizes either individual trees or a grove of island oak. Notice how the oaks in Santa Rosa are found mainly in the central valley and away from the coast. In total, there is a total of 1001 oak points on Santa Rosa. Out of these points, XX are seedlings and XX are adults."),
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
                 tabPanel("Fog",
                          # Select box with options for fog scenarios
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("fogscen", "Fog Scenarios", 
                                          choices = c("Constant", "Increase", "Decrease", "Elevation Threshold")),
                              # Input: Custom 30 yr periods format with basic animation
                              sliderTextInput("timeperiods","Time Periods" , 
                                              choices = c("1981-2010", "2010-2039", "2040-2069", "2070-2099"),
                                              animate = TRUE)
                            ),
                            
                            # Show maps of SRI and SCR with the chosen fog scenario with seperate tabs for each island
                            mainPanel(
                              tabsetPanel(
                                tabPanel("Santa Cruz",
                                         leafletOutput("scrfogmap", width=700, height=400)),
                                tabPanel("Santa Rosa",
                                         leafletOutput("srifogmap", width=700, height=400))
                              )
                            )
                          )),
                 
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
                                         h3("Projected Climate"),
                                         leafletOutput("SCRclimatemap", width=700, height=400),
                                         h3("Historic Climate"),
                                         leafletOutput("scrHC", width=700, height=400)),
                                tabPanel("Santa Rosa",
                                         h3("Projected Climate"),
                                         leafletOutput("SRIclimatemap", width=700, height=400),
                                         h3("Historic Climate"),
                                         leafletOutput("sriHC", width=700, height=400))
                              )
                              
                            ),
                            
                            position = c("left", "right"),
                            fluid = FALSE
                          )
                          
                          
                 )
                 
)


server <- function(input, output) {
  
  points_color <- reactive({
    input$points_colors
    
  })
  
  # Read in the data
  combo <- read.csv("data/oaks/sri/combo.csv")
  scr_points <- read.csv("data/oaks/scr/all_4326.csv")
  
  output$SCRpoints <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$Esri.WorldImagery) %>% 
      setView(lng = -119.722862, lat = 34.020433, zoom = 11) %>% 
      addCircleMarkers(radius = 4, fillColor = points_color(), stroke = FALSE, fillOpacity = 0.5, data = scr_points, lng = ~POINT_X, lat = ~POINT_Y)
    
  })
  
  filteredData <- reactive({
    combo[ combo$Age == input$age, ]
  })
  
  # Read in the data
  combo <- read.csv("data/oaks/sri/combo.csv")
  scr_points <- read.csv("data/oaks/scr/all_4326.csv")
  
  output$SRIpoints <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$OpenStreetMap.Mapnik) %>% 
      setView(lng = -120.107103, lat = 33.968757, zoom = 11) 
    
  })
  
  
  observe({
    
    leafletProxy("SRIpoints") %>%
      clearMarkers() %>% 
      addCircleMarkers(data = filteredData(), lng = ~POINT_X, lat = ~POINT_Y, radius = 4, fillColor = points_color(), stroke = FALSE, fillOpacity = 0.4)
  })
  
  output$scrfogmap <- renderLeaflet({
    
    
    foggy_scen<-switch(input$fogscen,
                       "Constant"=scen<-"const", 
                       "Increase"=scen<-"inc", 
                       "Decrease"=scen<-"dec", 
                       "Elevation Threshold"=scen<-"elev")
    
    fog_time<-switch(input$timeperiods,
                     "1981-2010"=time<-"historic",
                     "2010-2039"=time<-"2010_2039",
                     "2040-2069"=time<-"2040_2069", 
                     "2070-2099"=time<-"2070_2099")
    
    
    fog_scr<-raster(paste0("data/fog/scr/",foggy_scen,"/", fog_time, ".tif"))
    proj4string(fog_scr) <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
    
    fog_stack_list <- list.dirs("data/fog/scr/", full.names = TRUE)
    
    fog_files <- fog_stack_list[grep(paste0(foggy_scen), fog_stack_list, fixed=T)]
    
    fog_files_list <- list.files(fog_files, full.names = TRUE)
    
    fog_files2 <- fog_files_list[grep(".tif", fog_files_list, fixed=T)]
    
    fog_stack <- stack(fog_files2)
    
    fog_pal <- colorNumeric(
      palette = "Blues",
      domain = values(fog_stack),
      na.color = NA
    )
    
    
    leaflet() %>% 
      addProviderTiles(providers$Esri.WorldImagery) %>% 
      setView(lng = -119.722862, lat = 34.020433, zoom = 11) %>% 
      addRasterImage(fog_scr, colors = fog_pal, opacity = 0.8) %>% 
      addLegend("topright", pal = fog_pal, values= values(fog_stack),
                title = "Probability",
                labFormat = labelFormat(transform=function(fog_scr) sort (fog_scr, decreasing=FALSE)))
    
    
  }) #end render leaflet
  
  output$srifogmap <- renderLeaflet({
    
    
    foggy_scen<-switch(input$fogscen,
                       "Constant"=scen<-"const", 
                       "Increase"=scen<-"inc", 
                       "Decrease"=scen<-"dec", 
                       "Elevation Threshold"=scen<-"elev")
    
    fog_time<-switch(input$timeperiods,
                     "1981-2010"=time<-"historic",
                     "2010-2039"=time<-"2010_2039",
                     "2040-2069"=time<-"2040_2069", 
                     "2070-2099"=time<-"2070_2099")
    
    
    fog_sri<-raster(paste0("data/fog/sri/",foggy_scen,"/", fog_time, ".tif"))
    proj4string(fog_sri) <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
    
    fog_stack_list <- list.dirs("data/fog/sri/", full.names = TRUE)
    
    fog_files <- fog_stack_list[grep(paste0(foggy_scen), fog_stack_list, fixed=T)]
    
    fog_files_list <- list.files(fog_files, full.names = TRUE)
    
    fog_files2 <- fog_files_list[grep(".tif", fog_files_list, fixed=T)]
    
    fog_stack <- stack(fog_files2)
    
    fog_pal <- colorNumeric(
      palette = "Blues",
      domain = values(fog_stack),
      na.color = NA
    )
    
    
    leaflet() %>% 
      addTiles() %>% 
      setView(lng = -120.107103, lat = 33.968757, zoom = 11) %>% 
      addRasterImage(fog_sri, colors = fog_pal, opacity = 0.8) %>% 
      addLegend("topright", pal = fog_pal, values= values(fog_stack),
                title = "Probability",
                labFormat = labelFormat(transform=function(fog_stack) sort (fog_stack, decreasing=FALSE)))
    
    
  })
  
  #end render leaflet
  
  
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
    climate_files2 <- dir(files, recursive=TRUE, full.names=TRUE, pattern = paste0(climate_var, ".tif"))
    
    climatehist_files <- list.files("data/climate/sri/historic", recursive=TRUE, full.names=TRUE, pattern = paste0(climate_var, ".tif"))
    
    climate_files <- c(climatehist_files, climate_files2)
    
    climate_stack <- stack(climate_files)
    
    climate_colors <- reactive({ # delete
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
    
    
    leaflet() %>% 
      addProviderTiles(providers$OpenStreetMap.Mapnik) %>% 
      setView(lng = -120.107103, lat = 33.968757, zoom = 11) %>% 
      addRasterImage(climate_sri, colors = pal, opacity = 0.8) %>% 
      addLegend("bottomright", pal = pal, values= values(climate_stack),
                title = climate_title())
     
    
  })
  
  output$sriHC <- renderLeaflet({
    
    
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
    
    
    
    climate_sri<-raster(paste0("data/climate/sri/historic/", climate_var, ".tif"))
    
    proj4string(climate_sri) <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
    
    climate_stack_list <- list.dirs("data/climate/sri/", recursive = TRUE, full.names = TRUE)
    files <- climate_stack_list[grep(paste0(climate_scen), climate_stack_list, fixed=T)]
    climate_files2 <- dir(files, recursive=TRUE, full.names=TRUE, pattern = paste0(climate_var, ".tif"))
    
    climatehist_files <- list.files("data/climate/sri/historic", recursive=TRUE, full.names=TRUE, pattern = paste0(climate_var, ".tif"))
    
    climate_files <- c(climatehist_files, climate_files2)
    
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
    
    
    leaflet() %>% 
      addProviderTiles(providers$OpenStreetMap.Mapnik) %>% 
      setView(lng = -120.107103, lat = 33.968757, zoom = 11) %>% 
      addRasterImage(climate_sri, colors = pal, opacity = 0.8) %>% 
      addLegend("bottomright", pal = pal, values= values(climate_stack),
                title = climate_title())
    
    
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
    climate_files2 <- dir(files, recursive=TRUE, full.names=TRUE, pattern = paste0(climate_var, ".tif"))
    
    climatehist_files <- list.files("data/climate/scr/historic", recursive=TRUE, full.names=TRUE, pattern = paste0(climate_var, ".tif"))
    
    climate_files <- c(climatehist_files, climate_files2)
    
    climate_stack <- stack(climate_files)
    
    climate_colors <- reactive({
      input$raster_color_climate
    })
    
    climate_pal <- colorNumeric(
      palette = climate_colors(),
      domain = values(climate_stack),
      na.color = NA,
      reverse = TRUE
    )
    
    climate_title <- reactive({
      input$climate_variable
    })
    
    
    
    
    
    
    leaflet() %>% 
      addProviderTiles(providers$OpenStreetMap.Mapnik) %>% 
      setView(lng = -119.722862, lat = 34.020433, zoom = 11) %>% 
      addRasterImage(climate_scr, colors = climate_pal, opacity = 0.8) %>% 
      addLegend("bottomright", pal = climate_pal, values= values(climate_stack),
                title = climate_title())
    
    
    
  })
  
  output$scrHC <- renderLeaflet({
    
    
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
    
    climate_scr<-raster(paste0("data/climate/scr/historic/", climate_var, ".tif"))
    
    proj4string(climate_scr) <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
    
    climate_stack_list <- list.dirs("data/climate/scr/", recursive = TRUE, full.names = TRUE)
    files <- climate_stack_list[grep(paste0(climate_scen), climate_stack_list, fixed=T)]
    climate_files2 <- dir(files, recursive=TRUE, full.names=TRUE, pattern = paste0(climate_var, ".tif"))
    
    climatehist_files <- list.files("data/climate/scr/historic", recursive=TRUE, full.names=TRUE, pattern = paste0(climate_var, ".tif"))
    
    climate_files <- c(climatehist_files, climate_files2)
    
    climate_stack <- stack(climate_files)
    
    climate_colors <- reactive({
      input$raster_color_climate
    })
    
    climate_pal <- colorNumeric(
      palette = climate_colors(),
      domain = values(climate_stack),
      na.color = NA,
      reverse = TRUE
    )
    
    climate_title <- reactive({
      input$climate_variable
    })
    
    leg_boobs = "CWD"
    

    
    leaflet() %>% 
      addProviderTiles(providers$OpenStreetMap.Mapnik) %>% 
      setView(lng = -119.722862, lat = 34.020433, zoom = 11) %>% 
      addRasterImage(climate_scr, colors = climate_pal, opacity = 0.8) %>% 
      addLegend("bottomright", pal = climate_pal, values= values(climate_stack),
                title = leg_boobs)
    
    

    
  })
  
  
  
  
}



# Run the application 
shinyApp(ui = ui, server = server)

