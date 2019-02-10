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
ui <- navbarPage("Hot details from this reclusive oak's secret Life!", theme = shinytheme("flatly"),
   
   # Application title
   tabPanel("Oak Points"),
   tabPanel("Fog Scenarios"),
   tabPanel("Climate Scenarios"),
   
   #adds tabs on the top for all of our sections
   
   
   # Select box with options for fog scenarios
   sidebarLayout(
      sidebarPanel(
        textInput("txt", "Text input:", "text here"),
        sliderInput("slider", "Slider input:", 1, 100, 30),
        actionButton("action", "Button"),
        actionButton("action2", "Button2", class = "btn-primary"), 
        sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30)
      ),
      
      # Show maps of SRI and SCR with the chosen fog scenario with seperate tabs for each island
      mainPanel(
        tabsetPanel(
          tabPanel("Santa Cruz"),
          tabPanel("Santa Rosa")
        ),
         plotOutput("distPlot")
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

