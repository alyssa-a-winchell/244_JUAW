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
                              selectInput("fog_scen", label = h3("Fog Scenarios"), 
                                          choices = list("Constant" = 1, "Increase" = 2, "Decrease" = 3, "Elevation Threshold" = 4), 
                                          selected = 1),
                              # Input: Custom 30 yr periods format with basic animation
                              sliderTextInput("Time","Time Periods" , 
                                              choices = c("1981 - 2010", "2010 - 2039", "2040 - 2069", "2070 - 2099"),
                                              animate = TRUE)
                            ),
                            
                            # Show maps of SRI and SCR with the chosen fog scenario with seperate tabs for each island
                            mainPanel(
                              tabsetPanel(
                                tabPanel("Santa Cruz",
                                         plotOutput("distPlot")),
                                tabPanel("Santa Rosa",
                                         plotOutput("distPlot"))
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
  
  output$value <- renderPrint({ input$fog_scen })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

