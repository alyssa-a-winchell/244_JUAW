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

