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
ui <- navbarPage("EXPLICIT: Sweaty Oak Nuts)", theme = shinytheme("flatly"),
                 
                 # Application title
                 tabPanel("Oak Points"),
                 tabPanel("Fog Scenarios"),
                 tabPanel("Climate Scenarios",
                          
                          sidebarLayout(
                            sidebarPanel(
                            selectInput("variable", "Choose an Environmental Variable:",
                                        choices = c("Climate Water Deficit (CWD)", "Precipitation (PPT)", "Minimum Winter Temperature", "Maximum Summer Temperature")),
                            selectInput("scenario", "Choose a Climate Scenario:",
                                        choices = c("MPI 4.5 (Warm, Wet)", "CCSM4 (Hot, Wet)", "MIROC 4.5 (Warm,Dry) ", "MIROC 8.5 (Hot, Dry)")),
                            sliderInput("range", "Range:",
                                        min = 2010, max = 2099,
                                        value = c(2010,2040), step = 30),
                            noUiSliderInput(
                              inputId = "time", label = "Select Time Period:",
                              min = 2010, max = 2099, step = 30,
                              value = c(2010, 2039), behaviour = "drag", limit = 30, margin = 30,
                              format = wNumbFormat(decimals = 0,
                                                   thousand = "",
                                                   prefix = "Year: ")),
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
                 
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
