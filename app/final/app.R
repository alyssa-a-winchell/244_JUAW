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
                                                        "Adults" = "adult"))
                                       )
                                       
                              )
                            )
                          )),
                 tabPanel("Fog",
                          # Select box with options for fog scenarios
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("fog_scen", label = h3("Fog Scenarios"), 
                                          choices = list("Constant" = 1, "Increase" = 2, "Decrease" = 3, "Elevation Threshold" = 4), 
                                          selected = 1),
                              
                              hr(),
                              fluidRow(column(3, verbatimTextOutput("value")))
                              
                            ),
                            
                            # Show maps of SRI and SCR with the chosen fog scenario with seperate tabs for each island
                            mainPanel(
                              tabsetPanel(
                                tabPanel("Santa Cruz"),
                                tabPanel("Santa Rosa")
                              ),
                              plotOutput("distPlot")
                            )
                          )),
                 tabPanel("Climate Scenarios",
                          
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("variable", "Choose an Environmental Variable:",
                                          choices = c("Climate Water Deficit (CWD)", "Precipitation (PPT)", "Minimum Winter Temperature", "Maximum Summer Temperature")),
                              selectInput("scenario", "Choose a Climate Scenario:",
                                          choices = c("MPI 4.5 (Warm, Wet)", "CCSM4 (Hot, Wet)", "MIROC 4.5 (Warm,Dry) ", "MIROC 8.5 (Hot, Dry)")),
                              sliderInput("range", "Range:",
                                          min = 2010, max = 2099,
                                          value = c(2010,2040), step = 30, dragRange = TRUE),
                              noUiSliderInput(
                                inputId = "time", label = "Select Time Period:",
                                min = 2010, max = 2099, step = 30,
                                value = c(2010, 2039), behaviour = "drag", limit = 30, margin = 30,
                                color = "#2C778F",
                                format = wNumbFormat(decimals = 0,
                                                     thousand = "",
                                                     prefix = "Year ")),
                              width = 4
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
                 
                 #adds tabs on the top for all of our sections
                 
                 
                 
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$value <- renderPrint({ input$fog_scen })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

