#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(
  plotlyOutput("plot"),
  verbatimTextOutput("event")
)

server <- function(input, output) {
  
  # renderPlotly() also understands ggplot2 objects!
  output$plot <- renderPlotly({
    plot_ly(mtcars, x = ~mpg, y = ~wt, marker = list(size = ~(mpg+wt),
                                                     color = ~wt,
                                                     line = list(color = 'rgba(152, 0, 0, .8)',
                                                                 width = 2))) %>%
      layout(title = 'Styled Scatter',
             yaxis = list(zeroline = FALSE),
             xaxis = list(zeroline = FALSE))
  })
  
  output$event <- renderPrint({
    d <- event_data("plotly_hover")
    if (is.null(d)) "Hover on a point!" else d
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

