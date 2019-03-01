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
library(shinyWidgets)
library(sf)
library(raster)
library(leaflet)

# Define UI for application that displays data for fog scenarios on SRI and SCR
ui <- navbarPage("Hot details from this slutty oak's secret Life!", theme = shinytheme("flatly"),
   
   # Panel title
   tabPanel("Oak Points"),
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
                           leafletOutput("scrmap", width=1000, height=500)),
                  tabPanel("Santa Rosa",
                           leafletOutput("srimap", width=1000, height=500))
                )
              )
            )),
   tabPanel("Climate")
   #above adds tabs on the top for all of our sections
   
   

)

# Define server logic required to call the correct raster
server <- function(input, output) {
  # Reactive expression to create data frame of all input values ----
  # sliderValues <- reactive({
  #   
  #   data.frame(
  #     Name = c("Time Period"),
  #     Value = as.character(c(input$time)),
  #     stringsAsFactors = FALSE)
  #   
  # })
  

  output$scrmap <- renderLeaflet({
    
    
    scen<-switch(input$fogscen,
                 "Constant"=scen<-"const", 
                 "Increase"=scen<-"inc", 
                 "Decrease"=scen<-"dec", 
                 "Elevation Threshold"=scen<-"elev")

    time<-switch(input$timeperiods,
                 "1981-2010"=time<-"historic",
                 "2010-2039"=time<-"2010_2039",
                 "2040-2069"=time<-"2040_2069", 
                 "2070-2099"=time<-"2070_2099")
    
    
    scr<-raster(paste0("data/fog/scr/",scen,"/", time, ".tif"))
    proj4string(scr) <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
    
    fog_stack_list <- list.dirs("data/fog/scr/", recursive = TRUE, full.names = TRUE)
    
    fog_files <- fog_stack_list[grep(paste0(scen), fog_stack_list, fixed=T)]
    
    fog_files_list <- list.files(fog_files, recursive = TRUE, full.names = TRUE)
    
    fog_stack <- stack(fog_files_list)
    
    fog_pal <- colorNumeric(
      palette = "Blues",
      domain = values(fog_stack),
      na.color = NA
    )
    
    
    leaflet() %>% 
      addProviderTiles(providers$Esri.WorldImagery) %>% 
      setView(lng = -119.722862, lat = 34.020433, zoom = 11) %>% 
      addRasterImage(scr, colors = fog_pal, opacity = 0.8) %>% 
      addLegend("topright", pal = fog_pal, values= values(fog_stack),
                title = "Probability of Fog Inundation",
                labFormat = labelFormat(transform=function(scr) sort (scr, decreasing=FALSE)))
                  
    
}) #end render leaflet
  
  output$srimap <- renderLeaflet({
    
    
    scen<-switch(input$fogscen,
                 "Constant"=scen<-"const", 
                 "Increase"=scen<-"inc", 
                 "Decrease"=scen<-"dec", 
                 "Elevation Threshold"=scen<-"elev")
    
    time<-switch(input$timeperiods,
                 "1981-2010"=time<-"historic",
                 "2010-2039"=time<-"2010_2039",
                 "2040-2069"=time<-"2040_2069", 
                 "2070-2099"=time<-"2070_2099")
    
    
    sri<-raster(paste0("data/fog/sri/",scen,"/", time, ".tif"))
    proj4string(sri) <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
    
    fog_stack_list <- list.dirs("data/fog/sri/", recursive = TRUE, full.names = TRUE)
    
    fog_files <- fog_stack_list[grep(paste0(scen), fog_stack_list, fixed=T)]
    
    fog_files_list <- list.files(fog_files, recursive = TRUE, full.names = TRUE)
    
    fog_stack <- stack(fog_files_list)
    
    fog_pal <- colorNumeric(
      palette = "Blues",
      domain = values(fog_stack),
      na.color = NA
    )
    
    
    leaflet() %>% 
      addProviderTiles(providers$Esri.WorldImagery) %>% 
      setView(lng = -120.107103, lat = 33.968757, zoom = 11) %>% 
      addRasterImage(sri, colors = fog_pal, opacity = 0.8) %>% 
      addLegend("topright", pal = fog_pal, values= values(fog_stack),
                title = "Probability of Fog Inundation",
                labFormat = labelFormat(transform=function(sri) sort (sri, decreasing=FALSE)))
    
    
  })
  
   #end render leaflet
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

