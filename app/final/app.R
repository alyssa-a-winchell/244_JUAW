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

# Define UI for application that displays data for fog scenarios on SRI and SCR
ui <- navbarPage("Let us tomenTELLYA about it!", theme = shinytheme("flatly"),
                 
                 # Application title
                 tabPanel("Oak Points",
                          
                          mainPanel(
                            tabsetPanel(
                              tabPanel("Santa Cruz"),
                              tabPanel("Santa Rosa",
                                       sidebarPanel(
                                         radioButtons("age", "Choose Age Group:",
                                                      c("Seedlings" = "seedsap",
                                                        "Adults" = "adult",
                                                        "All" = "all")),
                                         width = 5
                                       )
                                       
                              )
                            )
                          )
                          
                          
                          
                 ),
                 
                 tabPanel("Fog",
                          # Select box with options for fog scenarios
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("fogscen", label = h3("Fog Scenarios"), 
                                          choices = list("Constant" = 1, "Increase" = 2, "Decrease" = 3, "Elevation Threshold" = 4), 
                                          selected = 1),
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
                 tabPanel("Climate Scenarios",
                          
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("variable", "Choose an Environmental Variable:",
                                          choices = c("Climate Water Deficit (CWD)", "Precipitation (PPT)", "Minimum Winter Temperature", "Maximum Summer Temperature")),
                              selectInput("scenario", "Choose a Climate Scenario:",
                                          choices = c("MPI 4.5 (Warm, Wet)", "CCSM4 (Hot, Wet)", "MIROC 4.5 (Warm,Dry) ", "MIROC 8.5 (Hot, Dry)")),
                              # Input: Custom 30 yr periods format with basic animation
                              sliderTextInput("Time","Time Periods" , 
                                              choices = c("1981 - 2010", "2010 - 2039", "2040 - 2069", "2070 - 2099"),
                                              animate = TRUE)
                            ),
                            
                            mainPanel(
                              tabsetPanel(
                                tabPanel("Santa Cruz"),
                                tabPanel("Santa Rosa")
                              )
                            ),
                            
                            position = c("left", "right"),
                            fluid = FALSE
                          )
                          
                          
                 )
                 
                 
                 
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$scrmap <- renderLeaflet({
    
    
    scen<-switch(input$fogscen,
                 "Constant Fog"=scen<-"const", 
                 "Fog Increase"=scen<-"inc", 
                 "Fog Decrease"=scen<-"dec", 
                 "Fog Elevation Threshold"=scen<-"elev")
    
    time<-switch(input$timeperiods,
                 "1981-2010"=time<-"historic",
                 "2010-2039"=time<-"2010_2039",
                 "2040-2069"=time<-"2040_2069", 
                 "2070-2099"=time<-"2070_2099")
    
    
    scr<-raster(paste0("data/fog/scr/",scen,"/", time, ".tif")) 
    proj4string(scr) <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
    
    
    leaflet() %>% addTiles() %>%
      addRasterImage(scr, colors = "Blues", opacity = 0.8) %>%
      #setView(lng=-13388304, lat=4012916, zoom=20) %>% #Figure out how to set view extent
      addLegend("topright", pal = "Blues", values = values(scr),
                title = "Probability of Fog Inundation", 
                labFormat = labelFormat(transform=function(scr) sort (scr, decreasing=FALSE))) #decreasing false until can figure out how to reverse legend colors
    
  }) #end render leaflet
  
  output$srimap <- renderLeaflet({
    
    
    scen<-switch(input$fogscen,
                 "Constant Fog"=scen<-"const", 
                 "Fog Increase"=scen<-"inc", 
                 "Fog Decrease"=scen<-"dec", 
                 "Fog Elevation Threshold"=scen<-"elev")
    
    time<-switch(input$timeperiods,
                 "1981-2010"=time<-"historic",
                 "2010-2039"=time<-"2010_2039",
                 "2040-2069"=time<-"2040_2069", 
                 "2070-2099"=time<-"2070_2099")
    
    setwd("G:/data/GitHub/244_JUAW/app/")
    sri<-raster(paste0("data/fog/sri/",scen,"/", time, ".tif")) 
    proj4string(sri) <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
    
    
    leaflet() %>% addTiles() %>%
      addRasterImage(sri, colors = "Blues", opacity = 0.8) %>%
      #setView(lng=-13388304, lat=4012916, zoom=20) %>% #Figure out how to set view extent
      addLegend("topright", pal = "Blues", values = values(sri),
                title = "Probability of Fog Inundation", 
                labFormat = labelFormat(transform=function(sri) sort (sri, decreasing=FALSE))) #decreasing false until can figure out how to reverse legend colors
    
  }) #end render leaflet
  
}

# Run the application 
shinyApp(ui = ui, server = server)

