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
ui <- navbarPage("Hot details from this slutty oak's secret Life!", theme = shinytheme("flatly"),
   
   # Panel title
   tabPanel("Oak Points"),
   tabPanel("Fog",
            # Select box with options for fog scenarios
            sidebarLayout(
              sidebarPanel(
                selectInput("fog_scen", label = h3("Fog Scenarios"), 
                            choices = list("Constant" = 1, "Increase" = 2, "Decrease" = 3, "Elevation Threshold" = 4), 
                            selected = 1),
                # Input: Custom currency format for with basic animation ----
                sliderInput("time", "Time Periods",
                            min = 1981, max = 2099,
                            value = 0, step = 30,
                            pre = "$", sep = ",",
                            animate = TRUE)
                
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
   tabPanel("Climate")
   #above adds tabs on the top for all of our sections
   
   

)

# Define server logic required to call the correct raster
server <- function(input, output) {
  # Reactive expression to create data frame of all input values ----
  sliderValues <- reactive({
    
    data.frame(
      Name = c("Time Period"),
      Value = as.character(c(input$time)),
      stringsAsFactors = FALSE)
    
  })
  

  # Show the values in an HTML table ----
  output$values <- renderTable({
    sliderValues()
  })
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

