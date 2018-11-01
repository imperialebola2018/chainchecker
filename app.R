#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Calculate onset date"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      dateInput("death",
                "Date of death:",
                value = Sys.Date()),
      numericInput("symptomatic",
                   "Duration of symptomatic period:",
                   value = 7),
      numericInput("incubation",
                   "Duration of incubation period:",
                   value = 21),
      textInput("id",
                "Identifier:",
                value = "ID1")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("onsetPlot"),
      textOutput("earliest_onset")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$onsetPlot <- renderPlot({
    
    df = data.frame("ID" = input$id,
                    "Death" = input$death,
                    "Symptomatic" = input$death - input$symptomatic,
                    "Exposure" = input$death - input$symptomatic- input$incubation)
    
    g = ggplot(df) 
    g = g + geom_rect(aes(xmin = Exposure,
                          xmax = Symptomatic,
                          ymin = ID, 
                          ymax = ID,
                          color = "Incubation"),
                      size = 1.1)
    g = g + geom_rect(aes(xmin = Symptomatic,
                          xmax = Death,
                          ymin = ID, 
                          ymax = ID,
                          color = "Symptomatic"),
                      size = 1.1)+
      geom_point( aes( x = Death,
                       y = ID,
                       color = "Death"),
                  size = 5) +
      geom_point(aes(x = Symptomatic,
                     y = ID,
                     color = "Earliest possible onset"),
                 size = 5) +
      ylab("Identifier") +
      theme(panel.background = element_rect(fill = "white", colour = "grey50"),
            text = element_text(size = 14),
            axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_x_date(date_breaks = "1 week", date_labels = "%b %d")+
      xlab("Date")
    
    g
  })
  
  output$earliest_onset = renderText({
    paste0("Earliest possible onset of symptoms for these parameters is ", 
           format(input$death - input$symptomatic, format = "%d %B"), 
           ".")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

