#

library(shiny)
library(ggplot2)
library(gridExtra)



# Define server logic required to draw a histogram
function(input, output) {
  
  output$onset_plot <- renderPlot({
    
    df = data.frame("ID" = input$id,
                    "Death" = input$death,
                    "Symptomatic" = input$death - input$symptomatic,
                    "Exposure" = input$death - input$symptomatic- input$incubation,
                    "Reported_Onset" = input$report_onset)
    
    g = ggplot(df) 
    g = g + geom_rect(aes(xmin = Exposure,
                          xmax = Symptomatic,
                          ymin = ID, 
                          ymax = ID,
                          color = "Incubation period"),
                      size = 1.1)
    g = g + geom_rect(aes(xmin = Symptomatic,
                          xmax = Death,
                          ymin = ID, 
                          ymax = ID,
                          color = "Symptomatic period"),
                      size = 1.1)+
      geom_point( aes( x = Death,
                       y = ID,
                       color = "Death"),
                  size = 5) +
      geom_point(aes(x = Symptomatic,
                     y = ID,
                     color = "Earliest possible onset"),
                 size = 5) +
      geom_point(aes(x = Reported_Onset,
                     y = ID,
                     color = "Reported onset"),
                 size = 5, shape = 4, stroke = 2) +
      ylab("Identifier") +
      labs(colour = "Key")+
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
  
  output$prob_intro = renderText({
    "We may be interested in how likely the reported onset date is, 
    given the date of death and the average incubation and symptomatic 
    periods as shown. To calculate this, we assign probability 
    distributions to both periods and use these to calculate the 
    probability of the reported onset given the observed date of death."
  })
  
  output$dist_plot = renderPlot({
    
    df_dist = data.frame("Days" = 0:50,
                         "Density_Symptomatic" = dlnorm(0:50,
                                                        meanlog = log(input$symptomatic),
                                                        sdlog = log(4)),
                         "Density_Incubation" = dlnorm(0:50,
                                                       meanlog = log(input$incubation),
                                                       sdlog = log(4)))
    
    g1 = ggplot(df_dist) + 
      geom_col( aes(x = Days, y = Density_Symptomatic, 
                    fill = "Symptomatic")) +
      geom_vline(xintercept = as.numeric(input$death - input$report_onset)) + 
      ylab("Density") +
      labs(fill = "Key")+
      theme(panel.background = element_rect(fill = "white", colour = "grey50"),
            text = element_text(size = 14),
            axis.text.x = element_text(angle = 45, hjust = 1)) +
      xlab("Duration") +
      ylim(0,0.1)
    
    g1
  })
  
}


