#Oakology Shiny App
# Find out more about building applications with Shiny here: http://shiny.rstudio.com/

#Notes
#Function for select box is selectInput.
#For slider it's sliderInput


# load the shiny package
library(shiny)
library(shinythemes) #perhaps need to install package while running too?
library(raster)
library(leaflet)
library(tidyverse)
library(sf)
library(RColorBrewer)

setwd("G:/data/GitHub/244_SMLW/Oakology")#Set wd just for running here, the app wd includes Oakology

ui<-fluidPage(theme = shinytheme("readable"),
              titlePanel("Oakology"),
              navbarPage("",
                         tabPanel("Summary", "Put in summary info"),
                         tabPanel("Example",
                                  sidebarPanel(
                                    fileInput("file", "File input:"),
                                    textInput("txt", "Text input:", "general"),
                                    sliderInput("slider", "Slider input:", 1, 100, 30),
                                    tags$h5("Deafult actionButton:"),
                                    actionButton("action", "Search"),
                                    
                                    tags$h5("actionButton with CSS class:"),
                                    actionButton("action2", "Action button", class = "btn-primary")
                                  ),
                                  mainPanel(
                                    tabsetPanel(
                                      tabPanel("Tab 1",
                                               h4("Table"),
                                               tableOutput("table"),
                                               h4("Verbatim text output"),
                                               verbatimTextOutput("txtout"),
                                               h1("Header 1"),
                                               h2("Header 2"),
                                               h3("Header 3"),
                                               h4("Header 4"),
                                               h5("Header 5")
                                      ),
                                      tabPanel("Tab 2", "This panel is intentionally left blank"),
                                      tabPanel("Tab 3", "This panel is intentionally left blank")
                                    )
                                  )
                         ),
                         tabPanel("Islands",
                                  mainPanel(
                                    selectInput("islandvar", "Choose an Island Variable", c("DEM", "Soil", "Vegetation")),
                                    leafletOutput("islandmap", width=1000, height=500)
                                  )
                         ),
                         tabPanel("SDM",
                                  mainPanel(
                                    selectInput("sdmcolor", "Choose a Color Palette", c("Spectral","Spectral2" ,"Viridis", "Magma")),
                                    selectInput("scenario", "Choose a Scenario", c("No Fog","Constant Fog", "Fog Increase", "Fog Decrease", "Fog Elevation Threshold")),
                                    selectInput("projection", "Choose a Projection", c("Hot-Wet","Warm-Wet", "Warm-Dry", "Hot-Dry")), #figure out how to make historic conditional for time period
                                    selectInput("timeperiod", "Choose a Time Period", c("2010-2039","2040-2069", "2070-2099")),
                                    leafletOutput("sdmmap", width=1000, height=500)
                                  )
                                  
                         )
              )
)

# Define server logic ----
server <- function(input, output) {
  
  #current wd is "G:/data/GitHub/244_SMLW" for all files
  
  
  output$sdmmap <- renderLeaflet({
    
    
    scen<-switch(input$scenario,
                 "No Fog"=scen<-"nofog",
                 "Constant Fog"=scen<-"fogconstant", 
                 "Fog Increase"=scen<-"foginc", 
                 "Fog Decrease"=scen<-"fogdec", 
                 "Fog Elevation Threshold"=scen<-"fogelev")
    proj<-switch(input$projection,
                 "Hot-Wet"=proj<-"CCSM4_rcp85",
                 "Warm-Wet"=proj<-"MPI_rcp45", 
                 "Warm-Dry"=proj<-"MIROC_rcp45", 
                 "Hot-Dry"=proj<-"MIROC_rcp85") #Need to include historic
    time<-switch(input$timeperiod,
                 "2010-2039"=time<-"_2010_2039",
                 "2040-2069"=time<-"_2040_2069", 
                 "2070-2099"=time<-"_2070_2099") #Need to include historic, if historic then no time period and time default defined as ""
    
    
    scr<-raster(paste0("data/sdm/scr/",scen,"/", proj, time, ".tif")) 
    proj4string(scr) <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
    
    
    sri<-raster(paste0("data/sdm/sri/",scen,"/", proj, time, ".tif")) 
    proj4string(sri) <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
    
    merged<-merge(scr, sri)
    
    sdmcol <- switch(input$sdmcolor,
                     "Spectral" = colorNumeric(palette = "Spectral", domain=values(merged), na.color = "transparent", reverse=TRUE),
                     "Spectral2" = colorNumeric(palette = "Spectral", domain=values(merged), na.color = "transparent", reverse=FALSE),
                     "Viridis" = colorNumeric(palette = "viridis", domain=values(merged), na.color = "transparent", reverse=TRUE),
                     "Magma" = colorNumeric(palette = "magma", domain=values(merged), na.color = "transparent", reverse=TRUE))
    
    leaflet() %>% addTiles() %>%
      addRasterImage(merged, colors = sdmcol, opacity = 0.8) %>%
      #setView(lng=-13388304, lat=4012916, zoom=20) %>% #Figure out how to set view extent
      addLegend("topright", pal = sdmcol, values = values(merged),
                title = "Suitability", 
                labFormat = labelFormat(transform=function(merged) sort (merged, decreasing=FALSE))) #decreasing false until can figure out how to reverse legend colors
    
  }) #end render leaflet
  
  output$islandmap <- renderLeaflet({
    
    scrdem<-raster("data/islands/scr/DEM.tif") 
    proj4string(scrdem) <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
    sridem<-raster("data/islands/sri/DEM.tif") 
    proj4string(sridem) <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
    mergeddem<-raster::merge(scrdem, sridem, tolerance = 0.5)
    
    scrveg<-read_sf("data/islands/scr/veg.shp")
    #crs(scrveg) <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
    sriveg<-read_sf("data/islands/sri/veg.shp")
    #proj4string(sriveg) <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
    mergedveg<-merge(scrveg, sriveg)
    
    scrsoils<-read_sf("data/islands/scr/soils.shp")
    #proj4string(scrsoils) <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
    srisoils<-read_sf("data/islands/sri/soils.shp")
    #proj4string(srisoils) <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
    mergedsoils<-merge(scrsoils, srisoils)
    
    col <- colorNumeric(palette = "Spectral", domain=values(mergeddem), na.color = "transparent", reverse=TRUE)
    
    leaflet() %>% addTiles() %>%
      addRasterImage(mergeddem, colors = col, opacity = 0.8)
    
    
    
  }) #end render leaflet
  
  
}#end server

# Run the app ----
shinyApp(ui = ui, server = server)


