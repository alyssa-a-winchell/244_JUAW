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
ui <- navbarPage("Oak Nuts ;)", theme = shinytheme("flatly"),
                 
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
                 
                 
                 tabPanel("Fog Scenarios"),
                 tabPanel("Climate Scenarios")
                 
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

